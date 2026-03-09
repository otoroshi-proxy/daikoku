package fr.maif.daikoku.controllers

import com.dimafeng.testcontainers.GenericContainer.FileSystemBind
import com.dimafeng.testcontainers.lifecycle.and
import com.dimafeng.testcontainers.scalatest.TestContainersForAll
import com.dimafeng.testcontainers.GenericContainer
import fr.maif.daikoku.domain.*
import fr.maif.daikoku.testUtils.DaikokuSpecHelper
import org.scalatest.BeforeAndAfter
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.play.PlaySpec
import org.testcontainers.containers.BindMode
import cats.implicits.catsSyntaxOptionId

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class HomeControllerSpec()
    extends PlaySpec
    with DaikokuSpecHelper
    with IntegrationPatience
    with BeforeAndAfter
    with TestContainersForAll {

  override type Containers = GenericContainer and GenericContainer

  override def startContainers(): Containers = {
    val containerOto = GenericContainer
      .Def(
        dockerImage = "maif/otoroshi",
        exposedPorts = Seq(8080),
        fileSystemBind = Seq(
          FileSystemBind(
            s"$pwd/test/fr/maif/daikoku/otoroshi.json",
            "/home/user/otoroshi.json",
            BindMode.READ_ONLY
          )
        ),
        env = Map("APP_IMPORT_FROM" -> "/home/user/otoroshi.json")
      )
      .start()
    val containerMailer = GenericContainer
      .Def(
        dockerImage = "reachfive/fake-smtp-server",
        exposedPorts = Seq(1080, 1025)
      )
      .start()
    containerOto and containerMailer
  }

  before {
    startContainers()
  }

  val pwd = System.getProperty("user.dir");

  "a daikoku admin" can {
    "check tenant health " in withContainers {

      case (otoroshi: GenericContainer) and (mailer: GenericContainer) =>
        println(otoroshi.mappedPort(8080))
        println(mailer.mappedPort(1025))

        setupEnvBlocking(
          tenants = Seq(
            tenant.copy(mailerSettings =
              SimpleSMTPSettings(
                host = "http://localhost",
                port = mailer.mappedPort(1025).toString,
                fromTitle = "testMailerSMTP",
                fromEmail = "noreply@test.io",
                template = None,
                username = None,
                password = None
              ).some
            )
          ),
          users = Seq(daikokuAdmin, tenantAdmin, user),
          teams = Seq(defaultAdminTeam),
          apis = Seq(adminApi)
        )

        val session = loginWithBlocking(daikokuAdmin, tenant)
        println(tenant)
        val respAvecMailer = httpJsonCallBlocking(
          path = s"/health"
        )(tenant, session)
        respAvecMailer.status mustBe 200
        respAvecMailer.json.toString mustBe "{\"datastore\":\"UP\",\"Test Corp.\":{\"tenantMode\":\"Default\"}}"
        mailer.stop()

        val respSansMailer = httpJsonCallBlocking(
          path = s"/health"
        )(tenant, session)
        println(respSansMailer.json.toString)
        respAvecMailer.json.toString mustBe "{\"datastore\":\"UP\",\"Test Corp.\":{\"tenantMode\":\"Default\"}}"
    }
  }
}
