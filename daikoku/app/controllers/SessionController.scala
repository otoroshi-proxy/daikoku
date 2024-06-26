package fr.maif.otoroshi.daikoku.ctrls

import fr.maif.otoroshi.daikoku.actions.DaikokuAction
import fr.maif.otoroshi.daikoku.audit.AuditTrailEvent
import fr.maif.otoroshi.daikoku.ctrls.authorizations.async.{
  DaikokuAdminOnly,
  PublicUserAccess
}
import fr.maif.otoroshi.daikoku.env.Env
import org.joda.time.DateTime
import play.api.libs.json.{JsArray, Json}
import play.api.mvc._

import scala.concurrent.ExecutionContext

class SessionController(
    DaikokuAction: DaikokuAction,
    env: Env,
    cc: ControllerComponents
) extends AbstractController(cc) {

  implicit val ec: ExecutionContext = env.defaultExecutionContext
  implicit val ev: Env = env

  def allSessions() =
    DaikokuAction.async { ctx =>
      DaikokuAdminOnly(
        AuditTrailEvent("@{user.name} has accessed all sessions")
      )(ctx) {
        env.dataStore.userSessionRepo.findAll().map { users =>
          Ok(JsArray(users.map(_.asJson)))
        }
      }
    }
  def deleteAllSessions() =
    DaikokuAction.async { ctx =>
      DaikokuAdminOnly(
        AuditTrailEvent("@{user.name} has deleted all sessions")
      )(ctx) {
        env.dataStore.userSessionRepo.deleteAll().map { _ =>
          Ok(Json.obj("done" -> true))
        }
      }
    }
  def deleteSession(id: String) =
    DaikokuAction.async { ctx =>
      DaikokuAdminOnly(
        AuditTrailEvent(s"@{user.name} has deleted session with id ${id}")
      )(ctx) {
        env.dataStore.userSessionRepo.deleteById(id).map { _ =>
          Ok(Json.obj("done" -> true))
        }
      }
    }

  def sessionRenew() =
    DaikokuAction.async { ctx =>
      PublicUserAccess(
        AuditTrailEvent(s"@{user.name} has renewed its session")
      )(ctx) {
        val sessionMaxAge = (ctx.tenant.authProviderSettings \ "sessionMaxAge")
          .asOpt[Int]
          .getOrElse(86400)
        val session =
          ctx.session.copy(expires = DateTime.now().plusSeconds(sessionMaxAge))
        env.dataStore.userSessionRepo.save(session).map { _ =>
          Ok(
            session.asSimpleJson
          ).withSession("sessionId" -> session.sessionId.value)
        }
      }
    }
}
