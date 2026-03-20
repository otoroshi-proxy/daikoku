package fr.maif.daikoku.jobs

import cats.data.EitherT
import cats.implicits.*
import cats.syntax.option.*
import fr.maif.daikoku.audit.{ApiKeyRotationEvent, JobEvent}
import fr.maif.daikoku.controllers.AppError
import fr.maif.daikoku.domain.*
import fr.maif.daikoku.domain.NotificationAction.{
  OtoroshiSyncApiError,
  OtoroshiSyncSubscriptionError
}
import fr.maif.daikoku.domain.json.{
  ApiSubscriptionyRotationFormat,
  OtoroshiApiKeyFormat,
  OtoroshiTargetFormat,
  TeamFormat
}
import fr.maif.daikoku.env.Env
import fr.maif.daikoku.storage.drivers.postgres.{
  Col,
  ColJson,
  ColJsonArray,
  PostgresDataStore
}
import fr.maif.daikoku.utils.*
import fr.maif.daikoku.utils.Time
import org.apache.pekko.actor.Cancellable
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Sink
import org.joda.time.DateTime
import play.api.Logger
import play.api.i18n.MessagesApi
import play.api.libs.json.*

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success



case class SyncAllSubscription()

case class SyncApiSubscriptionContext(subscription: ApiSubscription, api: Api, usagePlan: UsagePlan, by: User)

object LongExtensions {
  implicit class HumanReadableExtension(duration: Long) {
    final def toHumanReadable: String = {
      val units = Seq(
        TimeUnit.DAYS,
        TimeUnit.HOURS,
        TimeUnit.MINUTES,
        TimeUnit.SECONDS,
        TimeUnit.MILLISECONDS
      )

      val timeStrings = units
        .foldLeft((Seq.empty[String], duration))({
          case ((humanReadable, rest), unit) =>
            val name = unit.toString.toLowerCase()
            val result = unit.convert(rest, TimeUnit.NANOSECONDS)
            val diff = rest - TimeUnit.NANOSECONDS.convert(result, unit)
            val str = result match {
              case 0    => humanReadable
              case 1    => humanReadable :+ s"1 ${name.init}" // Drop last 's'
              case more => humanReadable :+ s"$more $name"
            }
            (str, diff)
        })
        ._1

      timeStrings.size match {
        case 0 => ""
        case 1 => timeStrings.head
        case _ => timeStrings.init.mkString(", ") + " and " + timeStrings.last
      }
    }
  }
}

case class ApiForSync(id: ApiId, name: String, metadata: Map[String, String])
object ApiForSync {
  def readFromJson(json: JsValue): ApiForSync = ApiForSync(
    id = ApiId((json \ "_id").as[String]),
    name = (json \ "name").as[String],
    metadata = (json \ "metadata").asOpt[Map[String, String]].getOrElse(Map.empty)
  )
}

case class UserForSync(id: UserId, name: String, email: String, metadata: Map[String, String])
object UserForSync {
  def readFromJson(json: JsValue): UserForSync = UserForSync(
    id = UserId((json \ "_id").as[String]),
    name = (json \ "name").as[String],
    email = (json \ "email").as[String],
    metadata = (json \ "metadata").asOpt[Map[String, String]].getOrElse(Map.empty)
  )
}

case class SubscriptionForSync(
                                apiKey: OtoroshiApiKey,
                                customMetadata: Option[JsObject],
                                metadata: Option[JsObject],
                                enabled: Boolean,
                                rotation: Option[ApiSubscriptionRotation],
                                validUntil: Option[DateTime]
                              )
object SubscriptionForSync {
  def readFromJson(json: JsValue): SubscriptionForSync = SubscriptionForSync(
    apiKey = (json \ "apiKey").as(using OtoroshiApiKeyFormat),
    customMetadata = (json \ "customMetadata").asOpt[JsObject],
    metadata = (json \ "metadata").asOpt[JsObject],
    enabled = (json \ "enabled").asOpt[Boolean].getOrElse(true),
    rotation = (json \ "rotation").asOpt(using ApiSubscriptionyRotationFormat),
    validUntil = (json \ "validUntil").asOpt[Long].map(l => new DateTime(l))
  )
}

case class PlanForSync(
                        id: UsagePlanId,
                        customName: String,
                        otoroshiTarget: Option[OtoroshiTarget],
                        maxPerSecond: Option[Long],
                        maxPerDay: Option[Long],
                        maxPerMonth: Option[Long],
                        autoRotation: Option[Boolean],
                        metadata: Map[String, String]
                      )
object PlanForSync {
  def readFromJson(json: JsValue): PlanForSync = PlanForSync(
    id = UsagePlanId((json \ "_id").as[String]),
    customName = (json \ "customName").asOpt[String].getOrElse(""),
    otoroshiTarget = (json \ "otoroshiTarget").asOpt(using OtoroshiTargetFormat),
    maxPerSecond = (json \ "maxPerSecond").asOpt[Long],
    maxPerDay = (json \ "maxPerDay").asOpt[Long],
    maxPerMonth = (json \ "maxPerMonth").asOpt[Long],
    autoRotation = (json \ "autoRotation").asOpt[Boolean],
    metadata = (json \ "metadata").asOpt[Map[String, String]].getOrElse(Map.empty)
  )
}

case class Child(subscription: SubscriptionForSync, api: ApiForSync, user: UserForSync, plan: PlanForSync) {

  private def metadataObjectToMap(
                                   obj: Map[String, JsValue]
                                 ): Map[String, String] = {
    obj.map {
      case (k, JsString(v)) => k -> v
      case (k, JsBoolean(v)) => k -> v.toString
      case (k, JsNumber(v)) => k -> v.toString
      case (k, JsNull) => k -> ""
      case (k, v) => k -> Json.stringify(v)
    }
  }

  def getContext(team: Team, tenant: Tenant): Map[String, String] = Map(
    "user.id" -> user.id.value,
    "user.name" -> user.name,
    "user.email" -> user.email,
    "api.id" -> api.id.value,
    "api.name" -> api.name,
    "plan.id" -> plan.id.value,
    "plan.name" -> plan.customName,
    "team.id" -> team.id.value,
    "team.name" -> team.name,
    "tenant.id" -> tenant.id.value,
    "tenant.name" -> tenant.name,
    "client.id" -> subscription.apiKey.clientId,
    "client.name" -> subscription.apiKey.clientName
  ) ++
    team.metadata.map(t => ("team.metadata." + t._1, t._2)) ++
    user.metadata.map(t => ("user.metadata." + t._1, t._2)) ++
    plan.metadata.map(t => ("plan.metadata." + t._1, t._2)) ++
    api.metadata.map(t => ("api.metadata." + t._1, t._2))

  def computeTags(team: Team, tenant: Tenant): Set[String] = {
    val planTags = plan.otoroshiTarget
      .flatMap(_.apikeyCustomization.tags.asOpt[Set[String]])
      .getOrElse(Set.empty[String])

    planTags.map(OtoroshiTarget.processValue(_, getContext(team, tenant)))
  }

  def computeMetadata(team: Team, tenant: Tenant): Map[String, String] = {
    val planMeta = metadataObjectToMap(
      plan.otoroshiTarget
        .flatMap(
          _.apikeyCustomization.metadata.asOpt[Map[String, JsValue]]
        )
        .getOrElse(Map.empty[String, JsValue])
    )

    val customMetaFromSub = metadataObjectToMap(
      subscription.customMetadata
        .flatMap(_.asOpt[Map[String, JsValue]])
        .getOrElse(Map.empty[String, JsValue])
    )

    val metadataFromSub = metadataObjectToMap(
      subscription.metadata
        .flatMap(_.asOpt[Map[String, JsValue]])
        .getOrElse(Map.empty[String, JsValue])
    )

    val newMetaFromDk =
      (planMeta ++ customMetaFromSub ++ metadataFromSub).map {
        case (a, b) => a -> OtoroshiTarget.processValue(b, getContext(team, tenant))
      }

    newMetaFromDk
  }

  def asOtoroshiApikey(team: Team, tenant: Tenant): ActualOtoroshiApiKey = {
    val maybeTarget: Option[OtoroshiTarget] = plan.otoroshiTarget
    val maybeCustomization: Option[ApikeyCustomization] = maybeTarget.map(_.apikeyCustomization)
    val meta = computeMetadata(team, tenant)
    val tags = computeTags(team, tenant)

    ActualOtoroshiApiKey(
      clientId = subscription.apiKey.clientId,
      clientSecret = subscription.apiKey.clientSecret,
      clientName = subscription.apiKey.clientName,
      authorizedEntities = maybeTarget.flatMap(t => t.authorizedEntities).getOrElse(AuthorizedEntities()),
      enabled = subscription.enabled,
      allowClientIdOnly = maybeCustomization.exists(_.clientIdOnly),
      readOnly = maybeCustomization.exists(_.readOnly),
      constrainedServicesOnly = maybeCustomization.exists(_.constrainedServicesOnly),
      throttlingQuota = plan.maxPerSecond.getOrElse(RemainingQuotas.MaxValue),
      dailyQuota = plan.maxPerDay.getOrElse(RemainingQuotas.MaxValue),
      monthlyQuota = plan.maxPerMonth.getOrElse(RemainingQuotas.MaxValue),
      tags = tags,
      metadata = meta ++ Map(
        "daikoku__metadata" -> meta.keys.mkString(" | "),
        "daikoku__tags" -> tags.mkString(" | ")
      ),
      restrictions = maybeCustomization.map(_.restrictions).getOrElse(ApiKeyRestrictions()),
      rotation = subscription.rotation.map(r =>
          ApiKeyRotation(
            enabled = r.enabled || plan.autoRotation.exists(e => e),
            rotationEvery = r.rotationEvery,
            gracePeriod = r.gracePeriod
          ))
        .orElse(plan.autoRotation.map(enabled => ApiKeyRotation(enabled = enabled))),
      validUntil = subscription.validUntil.map(_.getMillis),
    )
  }
}
object Child {
  def readFromJson(json: JsValue): Child = {
    Child(
      subscription = SubscriptionForSync.readFromJson((json \ "subscription").as[JsObject]),
      api = ApiForSync.readFromJson((json \ "api").as[JsObject]),
      user = UserForSync.readFromJson((json \ "user").as[JsObject]),
      plan = PlanForSync.readFromJson((json \ "plan").as[JsObject]),
    )
  }
}

case class SyncInformation(
                            parent: SyncApiSubscriptionContext,
                            childs: Seq[SyncApiSubscriptionContext],
                            team: Team,
                            apk: ActualOtoroshiApiKey,
                            otoroshiSettings: OtoroshiSettings,
                            tenant: Tenant,
                            tenantAdminTeam: Team
                          )

class OtoroshiVerifierJob(
                           client: OtoroshiClient,
    env: Env,
    translator: Translator,
    messagesApi: MessagesApi
) {

  private val synclogger = Logger("APIkey-Synchronizer")
  private val rotationlogger = Logger("APIkey-Rotation-Synchronizer")

  private val ref = new AtomicReference[Cancellable]()

  implicit val ec: ExecutionContext = env.defaultExecutionContext
  implicit val mat: Materializer = env.defaultMaterializer
  implicit val ev: Env = env
  implicit val me: MessagesApi = messagesApi
  implicit val tr: Translator = translator

  private val jobUser = User(
    id = UserId("otoroshi-verifier-job"),
    tenants = Set(),
    origins = Set(),
    name = "Otoroshi Verifier Job",
    email = "verifier@daikoku.io",
    picture = "https://www.otoroshi.io/assets/images/svg/otoroshi_logo.svg",
    password = None,
    lastTenant = None,
    personalToken = Some(IdGenerator.token(32)),
    defaultLanguage = None
    // lastTeams = Map.empty
  )

  def getListFromMeta(
                       key: String,
                       metadata: Map[String, String]
                     ): Set[String] = {
    metadata
      .get(key)
      .map(_.split('|').toSeq.map(_.trim).toSet)
      .getOrElse(Set.empty)
  }

  def mergeMetaValue(
                      key: String,
                      meta1: Map[String, String],
                      meta2: Map[String, String]
                    ): String = {
    val list1 = getListFromMeta(key, meta1)
    val list2 = getListFromMeta(key, meta2)
    (list1 ++ list2).mkString(" | ")
  }

  case class ComputedInformation(
                                  parent: ApiSubscription,
                                  childs: Seq[ApiSubscription],
                                  apk: ActualOtoroshiApiKey,
                                  computedApk: ActualOtoroshiApiKey,
                                  otoroshiSettings: OtoroshiSettings,
                                  tenant: Tenant,
                                  tenantAdminTeam: Team
                                )

  def start(): Unit = {
    if (
      !env.config.otoroshiSyncByCron && env.config.otoroshiSyncMaster && ref
        .get() == null
    ) {
      ref.set(
        env.defaultActorSystem.scheduler
          .scheduleAtFixedRate(10.seconds, env.config.otoroshiSyncInterval) {
            () =>// TODO - manage errors
               env.dataStore.tenantRepo.findAllNotDeleted()
                .map(tenants => Future.sequence(tenants.map(tenant =>run(SyncAllSubscription(), tenant))))
                .map(_ => ())
          }
      )
    }
  }

  def stop(): Unit = {
    Option(ref.get()).foreach(_.cancel())
  }

  private def sendErrorNotification(
                                     err: OtoroshiSyncNotificationAction,
                                     teamId: TeamId,
                                     tenantId: TenantId,
                                     otoHost: Option[String] = None
                                   ): Unit = {
    env.dataStore.notificationRepo
      .forTenant(tenantId)
      .save(
        Notification(
          id = NotificationId(IdGenerator.token(32)),
          tenant = tenantId,
          team = Some(teamId),
          sender = jobUser.asNotificationSender,
          date = DateTime.now(),
          notificationType = NotificationType.AcceptOnly,
          status = NotificationStatus.Pending(),
          action = err.asInstanceOf[NotificationAction]
        )
      )
    env.dataStore.tenantRepo.findByIdNotDeleted(tenantId).andThen {
      case Success(Some(tenant)) =>
        JobEvent(err.message).logJobEvent(
          tenant,
          jobUser,
          err match {
            case e: OtoroshiSyncSubscriptionError =>
              Json.obj(
                "subscription" -> e.subscription.asJson,
                "team" -> teamId.value,
                "tenant" -> tenantId.value
              )
            case e: OtoroshiSyncApiError =>
              Json.obj(
                "api" -> e.api.asJson,
                "team" -> teamId.value,
                "tenant" -> tenantId.value
              )
          }
        )
    }
    env.dataStore.userRepo
      .findAllNotDeleted()
      .map(_.filter(_.isDaikokuAdmin))
      .map { users =>
        def sendMail(mailer: Mailer, tenant: Tenant): Unit = {
          implicit val language: String = tenant.defaultLanguage.getOrElse("en")
          mailer.send(
            "Otoroshi synchronizer error",
            users.map(_.email),
            s"""<p>An error occured during the Otoroshi synchronization job for team ${teamId.value} on tenant ${tenantId.value} for $otoHost</p>
               |<p>${err.message}</p>
               |<strong>Details</strong>
               |<pre>${Json.prettyPrint(err.json)}</pre>
            """.stripMargin,
            tenant
          )
        }

        val tenants: Seq[TenantId] = users.flatMap(u => u.tenants).distinct
        env.dataStore.tenantRepo
          .find(
            Json.obj(
              "_deleted" -> false,
              "_id" -> Json.obj(
                "$in" -> JsArray(tenants.map(t => JsString(t.value)))
              )
            )
          )
          .map { tenants =>
            tenants.find { t =>
              t.mailerSettings.isDefined && t.mailerSettings.get.mailerType != "console"
            } match {
              case None =>
                sendMail(
                  new ConsoleMailer(ConsoleMailerSettings()),
                  tenants.head
                )
              case Some(tenant) => sendMail(tenant.mailer(using env), tenant)
            }
          }
      }
  }

  private def subscriptionFields(alias: String) =
    s"""json_build_object(
       |  '_id', $alias.content -> '_id',
       |  'apiKey', $alias.content -> 'apiKey',
       |  'customMetadata', $alias.content -> 'customMetadata',
       |  'metadata', $alias.content -> 'metadata',
       |  'enabled', $alias.content -> 'enabled',
       |  'rotation', $alias.content -> 'rotation',
       |  'validUntil', $alias.content -> 'validUntil',
       |  'team', $alias.content -> 'team',
       |  'createdAt', $alias.content -> 'createdAt'
       |)""".stripMargin

  private def apiFields(alias: String) =
    s"""json_build_object(
       |  '_id', $alias.content -> '_id',
       |  'name', $alias.content -> 'name',
       |  'metadata', $alias.content -> 'metadata'
       |)""".stripMargin

  private def userFields(alias: String) =
    s"""json_build_object(
       |  '_id', $alias.content -> '_id',
       |  'name', $alias.content -> 'name',
       |  'email', $alias.content -> 'email',
       |  'metadata', $alias.content -> 'metadata'
       |)""".stripMargin

  private def planFields(alias: String) =
    s"""json_build_object(
       |  '_id', $alias.content -> '_id',
       |  'customName', $alias.content -> 'customName',
       |  'otoroshiTarget', $alias.content -> 'otoroshiTarget',
       |  'maxPerSecond', $alias.content -> 'maxPerSecond',
       |  'maxPerDay', $alias.content -> 'maxPerDay',
       |  'maxPerMonth', $alias.content -> 'maxPerMonth',
       |  'autoRotation', $alias.content -> 'autoRotation',
       |  'metadata', $alias.content -> 'metadata'
       |)""".stripMargin

  private def getOtoroshiTarget(tenant: Tenant, usagePlan: PlanForSync): Option[OtoroshiSettings] = {
    usagePlan.otoroshiTarget
      .flatMap(target => tenant.otoroshiSettings.find(s => s.id == target.otoroshiSettings))
  }

  private def mergeOtoroshiApikeys(oldApiKey: ActualOtoroshiApiKey, newApikey: ActualOtoroshiApiKey, forceNewValue: Boolean = false): ActualOtoroshiApiKey = {
    oldApiKey.copy(
      enabled = true,
      validUntil = List(oldApiKey.validUntil, newApikey.validUntil).flatten.minOption,
      tags = oldApiKey.tags ++ newApikey.tags,
      metadata = oldApiKey.metadata ++
        newApikey.metadata ++
        Map(
          "daikoku__metadata" -> mergeMetaValue(
            "daikoku__metadata",
            oldApiKey.metadata,
            newApikey.metadata
          ),
          "daikoku__tags" -> mergeMetaValue(
            "daikoku__tags",
            oldApiKey.metadata,
            newApikey.metadata
          )
        ),
      restrictions = ApiKeyRestrictions(
        enabled =
          oldApiKey.restrictions.enabled || newApikey.restrictions.enabled,
        allowLast =
          oldApiKey.restrictions.allowLast || newApikey.restrictions.allowLast,
        allowed =
          oldApiKey.restrictions.allowed ++ newApikey.restrictions.allowed,
        forbidden =
          oldApiKey.restrictions.forbidden ++ newApikey.restrictions.forbidden,
        notFound =
          oldApiKey.restrictions.notFound ++ newApikey.restrictions.notFound
      ),
      authorizedEntities = AuthorizedEntities(
        groups =
          oldApiKey.authorizedEntities.groups | newApikey.authorizedEntities.groups,
        services =
          oldApiKey.authorizedEntities.services | newApikey.authorizedEntities.services,
        routes =
          oldApiKey.authorizedEntities.routes | newApikey.authorizedEntities.routes
      ),
      throttlingQuota = if (forceNewValue) newApikey.throttlingQuota else math.min(oldApiKey.throttlingQuota, newApikey.throttlingQuota),
      dailyQuota = if (forceNewValue) newApikey.dailyQuota else math.min(oldApiKey.dailyQuota, newApikey.dailyQuota),
      monthlyQuota = if (forceNewValue) newApikey.monthlyQuota else math.min(oldApiKey.monthlyQuota, newApikey.monthlyQuota),
      readOnly = oldApiKey.readOnly || newApikey.readOnly, //todo: not sure for aggregation
      allowClientIdOnly = oldApiKey.allowClientIdOnly || oldApiKey.allowClientIdOnly //todo: not sure for aggregation
    )
  }

  private def mergeAggregation(parent: Child, children: Seq[Child], team: Team, tenant: Tenant): Option[ActualOtoroshiApiKey] = {
    if (parent.subscription.enabled)
      children
        .foldLeft(parent.asOtoroshiApikey(team, tenant)) {
          case (acc, item) => mergeOtoroshiApikeys(acc, item.asOtoroshiApikey(team, tenant))
        }
        .some
    else
      None
  }

  private def clearApikey(apikey: ActualOtoroshiApiKey): ActualOtoroshiApiKey = {
    val metadata = apikey.metadata.get("daikoku__metadata").map(_.split("\\|").map(_.trim).toSeq).getOrElse(Seq.empty)
    val tags = apikey.metadata.get("daikoku__tags").map(_.split("\\|").map(_.trim).toSeq).getOrElse(Seq.empty)

    apikey.copy(
      metadata = apikey.metadata.removedAll(metadata),
      tags = apikey.tags.removedAll(tags)
    )
  }

  private def isEqual(apikey: ActualOtoroshiApiKey, apikeyFromSubscriptions: ActualOtoroshiApiKey): Boolean = {
    val daikokuMetaKeys = apikeyFromSubscriptions.metadata.getOrElse("daikoku__metadata", "").split("\\|").map(_.trim).filter(_.nonEmpty).toSet
    val metadataIsEqual = daikokuMetaKeys.forall(k =>
      apikey.metadata.get(k) == apikeyFromSubscriptions.metadata.get(k)
    ) && apikey.metadata.getOrElse("daikoku__metadata", "") == apikeyFromSubscriptions.metadata.getOrElse("daikoku__metadata", "")

    val tagsIsEqual = apikey.tags == apikeyFromSubscriptions.tags

    val restrictionsIsEqual = apikey.restrictions == apikeyFromSubscriptions.restrictions

    val rotationIsEqual = apikey.rotation == apikeyFromSubscriptions.rotation

    val throttlingQuotaIsEqual = apikey.throttlingQuota == apikeyFromSubscriptions.throttlingQuota
    val dailyQuotaIsEqual = apikey.dailyQuota == apikeyFromSubscriptions.dailyQuota
    val monthlyQuotaIsEqual = apikey.monthlyQuota == apikeyFromSubscriptions.monthlyQuota

    val authorizedEntitiesIsEqual = apikey.authorizedEntities == apikeyFromSubscriptions.authorizedEntities

    val readOnlyIsEqual = apikey.readOnly == apikeyFromSubscriptions.readOnly
    val allowClientIdOnlyIsEqual = apikey.allowClientIdOnly == apikeyFromSubscriptions.allowClientIdOnly

    metadataIsEqual &&
      tagsIsEqual &&
      restrictionsIsEqual &&
      rotationIsEqual &&
      dailyQuotaIsEqual &&
      monthlyQuotaIsEqual &&
      authorizedEntitiesIsEqual &&
      readOnlyIsEqual &&
      allowClientIdOnlyIsEqual
  }


  private def betterSynchronizeApikeys(
                                        entity: ApiId | UsagePlanId | ApiSubscriptionId | SyncAllSubscription = SyncAllSubscription(),
                                        tenant: Tenant,
                                        parallelism: Int = 25
                                      ): Future[Unit] = {

    val predicate: String = entity match {
      case apiId: ApiId =>
        s"""AND (s.content ->> 'api' = '${apiId.value}'
           |     OR s._id IN (SELECT content ->> 'parent' FROM api_subscriptions WHERE content ->> 'api' = '${apiId.value}' AND content ->> 'parent' IS NOT NULL))""".stripMargin
      case usagePlanId: UsagePlanId =>
        s"""AND (s.content ->> 'plan' = '${usagePlanId.value}'
           |     OR s._id IN (SELECT content ->> 'parent' FROM api_subscriptions WHERE content ->> 'plan' = '${usagePlanId.value}' AND content ->> 'parent' IS NOT NULL))""".stripMargin
      case subscriptionId: ApiSubscriptionId =>
        s"""AND (s._id = '${subscriptionId.value}'
           |     OR s._id = (SELECT content ->> 'parent' FROM api_subscriptions WHERE _id = '${subscriptionId.value}'))""".stripMargin
      case _: SyncAllSubscription => ""
    }

    val findAllSubscriptionsFromEntityStreamSql: String =
      s"""
         |SELECT json_build_object(
         |               'subscription', p.subscription,
         |               'api', p.api,
         |               'user', p."user",
         |               'plan', p.plan
         |       ) AS parent,
         |       COALESCE(json_agg(
         |        json_build_object(
         |                'subscription', ${subscriptionFields("s")},
         |                'api', ${apiFields("apis")},
         |                'user', ${userFields("users")},
         |                'plan', ${planFields("usage_plans")}
         |        )) FILTER (WHERE s._id IS NOT NULL), '[]'::json) AS children,
         |        teams.content AS team
         |FROM (
         |    SELECT s._id as parent_id, s.content ->> 'team' as team_id, s.content ->> 'createdAt' as created_at,
         |           (${subscriptionFields("s")})::jsonb as subscription, (${userFields("users")})::jsonb as "user", (${planFields("usage_plans")})::jsonb as plan, (${apiFields("apis")})::jsonb as api
         |    FROM api_subscriptions s
         |    INNER JOIN apis ON apis._id = s.content ->> 'api'
         |    INNER JOIN users ON users._id = s.content ->> 'by'
         |    INNER JOIN usage_plans ON usage_plans._id = s.content ->> 'plan'
         |    WHERE s.content ->> 'parent' IS NULL
         |      $predicate
         |) p
         |LEFT JOIN api_subscriptions s ON p.parent_id = s.content ->> 'parent' AND (s.content ->> 'enabled')::bool IS TRUE
         |LEFT JOIN apis ON apis._id = s.content ->> 'api'
         |LEFT JOIN users ON users._id = s.content ->> 'by'
         |LEFT JOIN usage_plans ON usage_plans._id = s.content ->> 'plan'
         |INNER JOIN teams ON teams._id = p.team_id
         |GROUP BY p.parent_id, p.created_at, p.subscription, p.api, p.plan, p.user, teams.content
         |ORDER BY p.created_at
         |""".stripMargin

    val processed = new java.util.concurrent.atomic.AtomicLong(0)
    val synced = new java.util.concurrent.atomic.AtomicLong(0)
    val skipped = new java.util.concurrent.atomic.AtomicLong(0)
    val errored = new java.util.concurrent.atomic.AtomicLong(0)

    val startTime = System.nanoTime()

    synclogger.debug(s"Starting fullyStreamedSync for tenant ${tenant.id.value} with parallelism=$parallelism")

    env.dataStore
      .asInstanceOf[PostgresDataStore]
      .queryRawMappedStream(
        findAllSubscriptionsFromEntityStreamSql,
        Seq(Col("parent", ColJson), Col("children", ColJsonArray), Col("team", ColJson)),
      )
      .mapAsyncUnordered(parallelism) { row =>
        val parent = Child.readFromJson((row \ "parent").as[JsObject])
        val children = (row \ "children")
          .asOpt[Seq[JsValue]]
          .map(_.filter(_ != JsNull).map(Child.readFromJson))
          .getOrElse(Seq.empty)
        val team = (row \ "team").as(using TeamFormat)
        val count = processed.incrementAndGet()

        if (count % 100 == 0) {
          val elapsed = (System.nanoTime() - startTime) / 1000000000.0
          val rate = count / elapsed
          synclogger.debug(f"Progress: $count processed ($synced synced, $skipped skipped, $errored errors) — $rate%.1f/s")
        }

        (for {
          otoroshiSettings <- EitherT.fromOption[Future](
            getOtoroshiTarget(tenant, parent.plan), AppError.EntityNotFound("otoroshi target"))
          apikey <- EitherT(client.getApikey(parent.subscription.apiKey.clientId)(using otoroshiSettings))
          apikeyFromSubscriptions <- EitherT.fromOption[Future](
            mergeAggregation(parent, children, team, tenant), AppError.EntityConflict("parent is not enabled"))
          equals = isEqual(apikey, apikeyFromSubscriptions)
          apk <- if (!equals) {
            val cleanApikey = clearApikey(apikey)
            val computedKey = mergeOtoroshiApikeys(cleanApikey, apikeyFromSubscriptions, forceNewValue = true)
            synclogger.debug(s"Updating apikey ${parent.subscription.apiKey.clientId} (${children.size} children)")
            EitherT(client.updateApiKey(key = computedKey)(using otoroshiSettings))
          } else {
            EitherT.pure[Future, AppError](apikey)
          }
        } yield (apk, equals)).value
          .recover {
            case e =>
              Left(AppError.InternalServerError(e.getMessage))
          }
          .map {
            case Left(error) =>
              errored.incrementAndGet()
              synclogger.error(s"Error synchronizing apikey ${parent.subscription.apiKey.clientId}: ${error.getErrorMessage()}")
            case Right((_, wasEqual)) =>
              if (wasEqual) skipped.incrementAndGet() else synced.incrementAndGet()
          }
      }
      .runWith(Sink.ignore)
      .map { _ =>
        val elapsed = (System.nanoTime() - startTime) / 1000000000.0
        synclogger.debug(f"Sync completed in $elapsed%.1fs — ${processed.get()} processed, ${synced.get()} synced, ${skipped.get()} skipped, ${errored.get()} errors")
      }
      .recover {
        case e =>
          val elapsed = (System.nanoTime() - startTime) / 1000000000.0
          synclogger.error(f"Sync stream failed after $elapsed%.1fs — ${processed.get()} processed, ${errored.get()} errors", e)
      }

  }

    def checkRotation(query: JsObject) = {
      for {
        allSubscriptions <-
          env.dataStore.apiSubscriptionRepo
            .forAllTenant()
            .findNotDeleted(query)
        // Get just parent sub (childs will be processed after)
        subscriptions <-
          env.dataStore.apiSubscriptionRepo
            .forAllTenant()
            .findNotDeleted(
              Json.obj(
                "_id" -> Json.obj(
                  "$in" -> JsArray(
                    Set
                      .from(
                        allSubscriptions
                          .map(s => s.parent.map(_.asJson).getOrElse(s.id.asJson))
                      )
                      .toSeq
                  )
                ),
                "rotation.enabled" -> true
              )
            )
      } yield {
        subscriptions.map(subscription =>
          for {
            tenant <- EitherT.fromOptionF(
              env.dataStore.tenantRepo.findByIdNotDeleted(subscription.tenant),
              sendErrorNotification(
                NotificationAction.OtoroshiSyncSubscriptionError(
                  subscription,
                  "Tenant does not exist anymore"
                ),
                subscription.team,
                subscription.tenant
              )
            )
            tenantAdminTeam <- EitherT.fromOptionF(
              env.dataStore.teamRepo
                .forTenant(tenant)
                .findOne(Json.obj("type" -> "Admin")),
              ()
            )
            api <- EitherT.fromOptionF(
              env.dataStore.apiRepo
                .forAllTenant()
                .findOneNotDeleted(
                  Json.obj(
                    "_id" -> subscription.api.value
                    //                  "state" -> ApiState.publishedJsonFilter
                  )
                ),
              sendErrorNotification(
                NotificationAction.OtoroshiSyncSubscriptionError(
                  subscription,
                  "API does not exist anymore"
                ),
                tenantAdminTeam.id,
                subscription.tenant
              )
            )
            plan <- EitherT.fromOptionF[Future, Unit, UsagePlan](
              env.dataStore.usagePlanRepo
                .forTenant(tenant)
                .findById(subscription.plan),
              sendErrorNotification(
                NotificationAction.OtoroshiSyncSubscriptionError(
                  subscription,
                  "Usage plan does not exist anymore"
                ),
                api.team,
                api.tenant
              )
            )
            otoroshiTarget <- EitherT.fromOption[Future](
              plan.otoroshiTarget,
              sendErrorNotification(
                NotificationAction.OtoroshiSyncSubscriptionError(
                  subscription,
                  "No Otoroshi target specified"
                ),
                api.team,
                api.tenant
              )
            )
            otoroshiSettings <- EitherT.fromOption[Future](
              tenant.otoroshiSettings
                .find(_.id == otoroshiTarget.otoroshiSettings),
              Seq(api.team, tenantAdminTeam.id)
                .map(team =>
                  sendErrorNotification(
                    NotificationAction.OtoroshiSyncSubscriptionError(
                      subscription,
                      "Otoroshi settings does not exist anymore"
                    ),
                    team,
                    api.tenant
                  )
                )
                .reduce((_, _) => ())
            )
            apk <- EitherT(
              client.getApikey(subscription.apiKey.clientId)(using otoroshiSettings)
            ).leftMap(e =>
              sendErrorNotification(
                NotificationAction.OtoroshiSyncSubscriptionError(
                  subscription,
                  s"Unable to fetch apikey from otoroshi: ${
                    Json
                      .stringify(AppError.toJson(e))
                  }"
                ),
                api.team,
                api.tenant,
                Some(otoroshiSettings.host)
              )
            )
          } yield {
            if (!apk.rotation.exists(r => r.enabled)) {
              client.updateApiKey(
                apk.copy(rotation = subscription.rotation.map(_.toApiKeyRotation))
              )(using otoroshiSettings)
            } else {
              val otoroshiNextSecret: Option[String] =
                apk.rotation.flatMap(_.nextSecret)
              val otoroshiActualSecret: String = apk.clientSecret
              val daikokuActualSecret: String = subscription.apiKey.clientSecret
              val pendingRotation: Boolean =
                subscription.rotation.exists(_.pendingRotation)

              var notification: Option[Notification] = None
              var newSubscription: Option[ApiSubscription] = None

              if (
                !pendingRotation && otoroshiNextSecret.isDefined && otoroshiActualSecret == daikokuActualSecret
              ) {
                rotationlogger.info(
                  s"rotation state updated to Pending for ${apk.clientName}"
                )
                newSubscription = subscription
                  .copy(
                    rotation =
                      subscription.rotation.map(_.copy(pendingRotation = true)),
                    apiKey = subscription.apiKey
                      .copy(clientSecret = otoroshiNextSecret.get)
                  )
                  .some
                notification = Notification(
                  id = NotificationId(IdGenerator.token(32)),
                  tenant = tenant.id,
                  team = Some(subscription.team),
                  sender = jobUser.asNotificationSender,
                  action = NotificationAction.ApiKeyRotationInProgressV2(
                    subscription = subscription.id,
                    api = api.id,
                    plan = plan.id
                  ),
                  notificationType = NotificationType.AcceptOnly
                ).some

                ApiKeyRotationEvent(subscription = subscription.id)
                  .logJobEvent(
                    tenant,
                    jobUser,
                    Json.obj("token" -> subscription.integrationToken)
                  )

              } else if (pendingRotation && otoroshiNextSecret.isEmpty) {
                rotationlogger.info(
                  s"rotation state updated to Ended for ${apk.clientName}"
                )
                notification = Notification(
                  id = NotificationId(IdGenerator.token(32)),
                  tenant = tenant.id,
                  team = Some(subscription.team),
                  sender = jobUser.asNotificationSender,
                  action = NotificationAction.ApiKeyRotationEndedV2(
                    subscription = subscription.id,
                    api = api.id,
                    plan = plan.id
                  ),
                  notificationType = NotificationType.AcceptOnly
                ).some
                newSubscription = subscription
                  .copy(
                    rotation =
                      subscription.rotation.map(_.copy(pendingRotation = true)),
                    apiKey = subscription.apiKey
                      .copy(clientSecret = otoroshiActualSecret)
                  )
                  .some

                ApiKeyRotationEvent(subscription = subscription.id)
                  .logJobEvent(
                    tenant,
                    jobUser,
                    Json.obj("token" -> subscription.integrationToken)
                  )
              }

              (newSubscription, notification) match {
                case (Some(subscription), Some(notification)) =>
                  for {
                    _ <-
                      env.dataStore.apiSubscriptionRepo
                        .forTenant(subscription.tenant)
                        .save(subscription)
                    aggSubs <-
                      env.dataStore.apiSubscriptionRepo
                        .forTenant(subscription.tenant)
                        .findNotDeleted(
                          Json.obj("parent" -> subscription.id.asJson)
                        )
                    _ <-
                      env.dataStore.apiSubscriptionRepo
                        .forTenant(subscription.tenant)
                        .updateManyByQuery(
                          Json.obj(
                            "_id" -> Json
                              .obj("$in" -> JsArray(aggSubs.map(_.id.asJson)))
                          ),
                          Json.obj(
                            "$set" -> Json.obj(
                              "rotation" -> subscription.rotation
                                .map(ApiSubscriptionyRotationFormat.writes)
                                .getOrElse(JsNull)
                                .as[JsValue]
                            )
                          )
                        )
                    _ <-
                      env.dataStore.notificationRepo
                        .forTenant(subscription.tenant)
                        .save(notification)
                  } yield ()
                case (_, _) =>
                  rotationlogger.info(s"no need to update rotation for ${apk.clientName}")
              }
            }
          }
        )
      }
    }

    // todo: pas 2 sychro en meme temps (select  for update)
    // checkRotation(query), //todo: sortir dans un autre job
    // verifyIfOtoroshiGroupsStillExists(),  //todo: sortir dans un autre job
    def run(entryPoint: ApiId | UsagePlanId | ApiSubscriptionId | SyncAllSubscription = SyncAllSubscription(), tenant: Tenant, parallelism: Int = 25): Future[Unit] = {
      synclogger.info(s"run apikey synchronisation with entry point as $entryPoint")

      val jobRepo = env.dataStore.JobInformationRepo.forTenant(tenant)
      val jobId = DatastoreId(s"sync-${IdGenerator.token(16)}")
      val now = DateTime.now()

      Time.concurrentTime(
        jobRepo
          .findOneNotDeleted(Json.obj(
            "jobName" -> JobName.ApiKeySynchronization.value,
            "status" -> JobStatus.Running.value))
          .flatMap {
            case Some(_) =>
              synclogger.info("can't run another ApiKeySynchronization, already one is running")
              Future.successful(())
            case None =>
              val jobInfo = JobInformation(
                id = jobId,
                tenant = tenant.id,
                jobName = JobName.ApiKeySynchronization,
                lockedBy = "otoroshi-verifier-job",
                lockedAt = now,
                expiresAt = now.plusHours(1),
                cursor = "",
                startedAt = now,
                lastBatchAt = now,
                status = JobStatus.Running
              )
              jobRepo.save(jobInfo).flatMap { _ =>
                betterSynchronizeApikeys(entryPoint, tenant, parallelism)
                  .flatMap { _ =>
                    synclogger.info("Sync verification ended")
                    jobRepo.save(jobInfo.copy(status = JobStatus.Completed, lastBatchAt = DateTime.now()))
                      .map(_ => ())
                  }
                  .recoverWith { case e =>
                    synclogger.error(s"Sync failed: ${e.getMessage}", e)
                    jobRepo.save(jobInfo.copy(status = JobStatus.Failed, lastBatchAt = DateTime.now()))
                      .map(_ => ())
                  }
              }
          }, "Synchronization run"
      )
    }
}
