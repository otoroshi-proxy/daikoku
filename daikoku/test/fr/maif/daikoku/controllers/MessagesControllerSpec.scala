package fr.maif.daikoku.controllers

import fr.maif.daikoku.domain.{DatastoreId, Message, MessageType, User, json}
import fr.maif.daikoku.testUtils.DaikokuSpecHelper
import fr.maif.daikoku.utils.IdGenerator
import org.joda.time.DateTime
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsArray, Json}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class MessagesControllerSpec()
    extends PlaySpec
    with DaikokuSpecHelper
    with IntegrationPatience {

  def adminMessage(
      user: User,
      sender: User,
      message: String,
      closed: Option[DateTime] = None
  ): Message =
    Message(
      id = DatastoreId(IdGenerator.token(128)),
      tenant = tenant.id,
      messageType = MessageType.Tenant(tenant.id),
      participants = Set(user.id) ++ defaultAdminTeam.users.map(_.userId),
      readBy = Set(user.id),
      chat = user.id,
      date = DateTime.now(),
      sender = sender.id,
      message = message,
      closed = closed,
      send = true
    )

  "a tenant admin" can {
    "close a chat" in {
      Await.result(waitForDaikokuSetup(), 5.second)
      setupEnvBlocking(
        tenants = Seq(tenant),
        users = Seq(tenantAdmin, user),
        teams = Seq(defaultAdminTeam),
        messages = Seq(adminMessage(user, user, "not closed", None))
      )
      val session = loginWithBlocking(tenantAdmin, tenant)

      val resp =
        httpJsonCallBlocking(
          path = s"/api/messages/${user.id.value}",
          method = "DELETE"
        )(using tenant, session)

      resp.status mustBe 200
    }
  }

  "a user" can {
    "get his message to admin team" in {
      setupEnvBlocking(
        tenants = Seq(tenant),
        users = Seq(tenantAdmin, user),
        messages = Seq(adminMessage(user, user, "1", None))
      )
      val session = loginWithBlocking(user, tenant)

      val respGet =
        httpJsonCallBlocking("/api/me/messages/admin")(using tenant, session)
      respGet.status mustBe 200
      val messages =
        json.SeqMessagesFormat.reads((respGet.json \ "messages").as[JsArray])
      messages.isSuccess mustBe true
      messages.get.length mustBe 1
      messages.get.head.message mustBe "1"
    }

    "get his previous messages" in {
      val closedDate = DateTime.now().minusHours(1)
      setupEnvBlocking(
        tenants = Seq(tenant),
        users = Seq(tenantAdmin, user),
        messages = Seq(adminMessage(user, user, "1", Some(closedDate)))
      )
      val session = loginWithBlocking(user, tenant)

      val respGetClosedDate = httpJsonCallBlocking(
        s"/api/messages/${user.id.value}/last-date"
      )(using tenant, session)
      respGetClosedDate.status mustBe 200
      val lastClosedDate = respGetClosedDate.json.as[Long]
      lastClosedDate mustBe closedDate.toDate.getTime

      val respGet = httpJsonCallBlocking(
        s"/api/me/messages?chat=${user.id.value}&date=$lastClosedDate"
      )(using tenant, session)
      respGet.status mustBe 200
      val messages =
        json.SeqMessagesFormat.reads((respGet.json \ "messages").as[JsArray])
      messages.isSuccess mustBe true

      messages.get.length mustBe 1
      messages.get.head.message mustBe "1"
    }

    "read his messages" in {
      setupEnvBlocking(
        tenants = Seq(tenant),
        users = Seq(tenantAdmin, user),
        messages = Seq(
          adminMessage(user, user, "1", None)
            .copy(date = DateTime.now().minusHours(1))
        )
      )
      val session = loginWithBlocking(tenantAdmin, tenant)

      val respGet =
        httpJsonCallBlocking(s"/api/me/messages")(using tenant, session)
      respGet.status mustBe 200
      val messages =
        (respGet.json \ "messages").as(using json.SeqMessagesFormat)
      messages.length mustBe 1
      messages.count(_.readBy.contains(tenantAdminId)) mustBe 0

      val respRead =
        httpJsonCallBlocking(
          path = s"/api/messages/${user.id.value}/_read",
          method = "PUT"
        )(using tenant, session)
      respRead.status mustBe 200

      val respVerif =
        httpJsonCallBlocking(s"/api/me/messages")(using tenant, session)
      respVerif.status mustBe 200
      val messagesVerif =
        (respVerif.json \ "messages").as(using json.SeqMessagesFormat)

      messagesVerif.length mustBe 1
      messagesVerif.count(_.readBy.contains(tenantAdminId)) mustBe 1
    }

    "send a message to admin team" in {
      setupEnvBlocking(
        tenants = Seq(tenant),
        users = Seq(tenantAdmin, user)
      )
      val session = loginWithBlocking(user, tenant)

      val respSend = httpJsonCallBlocking(
        path = "/api/messages/_send",
        method = "POST",
        body = Some(
          Json.obj(
            "message" -> "1",
            "participants" -> JsArray(
              (Set(user.id.asJson) ++ defaultAdminTeam.users
                .map(_.userId.asJson)).toSeq
            ),
            "chat" -> user.id.asJson
          )
        )
      )(using tenant, session)

      respSend.status mustBe 200

      val respGet =
        httpJsonCallBlocking("/api/me/messages")(using tenant, session)
      respGet.status mustBe 200
      val messages =
        json.SeqMessagesFormat.reads((respGet.json \ "messages").as[JsArray])
      messages.isSuccess mustBe true
      messages.get.length mustBe 1
      messages.get.head.message mustBe "1"
    }

    "not close a chat" in {
      setupEnvBlocking(
        tenants = Seq(tenant),
        users = Seq(tenantAdmin, user),
        teams = Seq(defaultAdminTeam),
        messages = Seq(adminMessage(user, user, "not closed", None))
      )

      val session = loginWithBlocking(user, tenant)

      val resp = httpJsonCallBlocking(
        path = s"/api/messages/${user.id.value}",
        method = "DELETE"
      )(using tenant, session)

      resp.status mustBe 403
    }
  }

  "a chat" must {
    "be closed after user self deletion" in {
      Await.result(waitForDaikokuSetup(), 5.second)
      setupEnvBlocking(
        tenants = Seq(tenant),
        users = Seq(tenantAdmin, user),
        teams = Seq(defaultAdminTeam),
        messages = Seq(adminMessage(user, user, "not closed", None))
      )
      val userSession = loginWithBlocking(user, tenant)
      val adminSession = loginWithBlocking(tenantAdmin, tenant)

      val respBefore =
        httpJsonCallBlocking("/api/me/messages")(using tenant, adminSession)
      respBefore.status mustBe 200
      (respBefore.json \ "messages").as[JsArray].value.nonEmpty mustBe true

      val respDelete =
        httpJsonCallBlocking(path = s"/api/me", method = "DELETE")(using
          tenant,
          userSession
        )
      respDelete.status mustBe 200

      val resp =
        httpJsonCallBlocking(
          path = s"/api/me/messages"
        )(using tenant, adminSession)

      resp.status mustBe 200
      (resp.json \ "messages").as[JsArray].value.isEmpty mustBe true
    }

    "be closed after user deletion" in {
      Await.result(waitForDaikokuSetup(), 5.second)
      setupEnvBlocking(
        tenants = Seq(tenant),
        users = Seq(daikokuAdmin, tenantAdmin, user),
        teams = Seq(defaultAdminTeam),
        messages = Seq(adminMessage(user, user, "not closed", None))
      )
      val userSession = loginWithBlocking(user, tenant)
      val adminSession = loginWithBlocking(tenantAdmin, tenant)
      val dkAdminSession = loginWithBlocking(daikokuAdmin, tenant)

      val respBefore =
        httpJsonCallBlocking("/api/me/messages")(using tenant, adminSession)
      respBefore.status mustBe 200
      (respBefore.json \ "messages").as[JsArray].value.nonEmpty mustBe true

      val respDelete =
        httpJsonCallBlocking(
          path = s"/api/admin/users/${userTeamUserId.value}",
          method = "DELETE"
        )(using tenant, dkAdminSession)
      logger.warn(Json.stringify(respDelete.json))
      respDelete.status mustBe 200

      val resp =
        httpJsonCallBlocking(
          path = s"/api/me/messages"
        )(using tenant, adminSession)

      resp.status mustBe 200
      (resp.json \ "messages").as[JsArray].value.size mustBe 0
    }
  }

}
