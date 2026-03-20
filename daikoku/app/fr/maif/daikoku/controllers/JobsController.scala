package fr.maif.daikoku.controllers

import fr.maif.daikoku.env.Env
import fr.maif.daikoku.jobs
import fr.maif.daikoku.utils.{DaikokuApiAction, OtoroshiClient}
import org.apache.pekko.http.scaladsl.util.FastFuture
import fr.maif.daikoku.jobs.{ApiKeyStatsJob, AuditTrailPurgeJob, OtoroshiVerifierJob, SyncAllSubscription}
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class JobsController(
    DaikokuApiAction: DaikokuApiAction,
    otoroshiVerifierJob: OtoroshiVerifierJob,
    apiKeyStatsJob: ApiKeyStatsJob,
    auditTrailPurgeJob: AuditTrailPurgeJob,
    env: Env,
    cc: ControllerComponents,
    otoroshiClient: OtoroshiClient
) extends AbstractController(cc) {

  implicit val ec: ExecutionContext = env.defaultExecutionContext
  implicit val ev: Env = env

  def otoroshiSyncJob(parallelism: Int = 25): Action[AnyContent] =
    DaikokuApiAction.async { ctx =>
      otoroshiVerifierJob.run(tenant = ctx.tenant, parallelism = parallelism)
        .map(_ => Ok(Json.obj("done" -> true)))
    }

  def apikeysStatsSyncJob(): Action[AnyContent] =
    Action.async { req =>
      if (env.config.apikeysStatsByCron) {
        apiKeyStatsJob.getStats.map(_ => Ok(Json.obj("done" -> true)))
      } else {
        FastFuture.successful(NotFound(Json.obj("error" -> "API not found")))
      }
    }

  def auditTrailPurgeRunJob(): Action[AnyContent] =
    Action.async { req =>
      if (env.config.apikeysStatsByCron) {
        auditTrailPurgeJob.purge().map(_ => Ok(Json.obj("done" -> true)))
      } else {
        FastFuture.successful(NotFound(Json.obj("error" -> "API not found")))
      }
    }
}
