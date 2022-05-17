package fr.maif.otoroshi.daikoku.domain

import akka.http.scaladsl.util.FastFuture
import cats.syntax.option._
import fr.maif.otoroshi.daikoku.domain.json.{SeqIssueIdFormat, SeqPostIdFormat, SeqTeamIdFormat, SetApiTagFormat, UsagePlanFormat}
import fr.maif.otoroshi.daikoku.env.Env
import fr.maif.otoroshi.daikoku.utils.ReplaceAllWith
import fr.maif.otoroshi.daikoku.utils.StringImplicits.BetterString
import org.joda.time.DateTime
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}

object OtoroshiTarget {
  val expressionReplacer = ReplaceAllWith("\\$\\{([^}]*)\\}")
  val logger = play.api.Logger("OtoroshiTarget")

  def processValue(value: String, context: Map[String, String]): String = {
    value match {
      case v if v.contains("${") =>
        scala.util.Try {
          OtoroshiTarget.expressionReplacer.replaceOn(value) { expression =>
            context.getOrElse(expression, "--")
          }
        } recover {
          case e =>
            OtoroshiTarget.logger.error(
              s"Error while parsing expression, returning raw value: $value",
              e)
            value
        } get
      case _ => value
    }
  }
}

case class OtoroshiTarget(
                           otoroshiSettings: OtoroshiSettingsId,
                           authorizedEntities: Option[AuthorizedEntities],
                           apikeyCustomization: ApikeyCustomization = ApikeyCustomization()
                         ) extends CanJson[OtoroshiTarget] {
  def asJson: JsValue = json.OtoroshiTargetFormat.writes(this)
  def processedMetadata(context: Map[String, String]): Map[String, String] = {
    apikeyCustomization.metadata
      .asOpt[Map[String, String]]
      .getOrElse(Map.empty[String, String])
      .view
      .mapValues(v => OtoroshiTarget.processValue(v, context))
      .toMap
  }
  def processedTags(context: Map[String, String]): Seq[String] = {
    apikeyCustomization.tags
      .asOpt[Seq[String]]
      .getOrElse(Seq.empty[String])
      .map(v => OtoroshiTarget.processValue(v, context))
  }
}

case class OtoroshiService(name: String,
                           otoroshiSettings: OtoroshiSettingsId,
                           service: OtoroshiServiceId)
  extends CanJson[OtoroshiService] {
  def asJson: JsValue = json.OtoroshiServiceFormat.writes(this)
}

trait BillingTimeUnit extends CanJson[BillingTimeUnit] {
  def name: String
  def asJson: JsValue = JsString(name)
}

object BillingTimeUnit {
  case object Hour extends BillingTimeUnit {
    def name: String = "Hour"
  }
  case object Day extends BillingTimeUnit {
    def name: String = "Day"
  }
  case object Month extends BillingTimeUnit {
    def name: String = "Month"
  }
  case object Year extends BillingTimeUnit {
    def name: String = "Year"
  }
  val values: Seq[BillingTimeUnit] =
    Seq(Hour, Day, Month, Year)
  def apply(name: String): Option[BillingTimeUnit] = name.toLowerCase() match {
    case "hour"   => Hour.some
    case "hours"  => Hour.some
    case "day"    => Day.some
    case "days"   => Day.some
    case "month"  => Month.some
    case "months" => Month.some
    case "year"   => Year.some
    case "years"  => Year.some
    case _        => None
  }
}

case class BillingDuration(value: Long, unit: BillingTimeUnit)
  extends CanJson[BillingDuration] {
  def asJson: JsValue = json.BillingDurationFormat.writes(this)
}

sealed trait ApiVisibility {
  def name: String
}

object ApiVisibility {
  case object Public extends ApiVisibility {
    def name: String = "Public"
  }
  case object PublicWithAuthorizations extends ApiVisibility {
    def name: String = "PublicWithAuthorizations"
  }
  case object Private extends ApiVisibility {
    def name: String = "Private"
  }
  case object AdminOnly extends ApiVisibility {
    def name: String = "AdminOnly"
  }
  val values: Seq[ApiVisibility] =
    Seq(Public, Private, PublicWithAuthorizations)
  def apply(name: String): Option[ApiVisibility] = name match {
    case "Public"                   => Public.some
    case "Private"                  => Private.some
    case "PublicWithAuthorizations" => PublicWithAuthorizations.some
    case "AdminOnly"                => AdminOnly.some
    case _                          => None
  }
}

sealed trait UsagePlanVisibility {
  def name: String
}

object UsagePlanVisibility {
  case object Public extends UsagePlanVisibility {
    def name: String = "Public"
  }

  case object Private extends UsagePlanVisibility {
    def name: String = "Private"
  }
  val values: Seq[UsagePlanVisibility] =
    Seq(Public, Private)
  def apply(name: String): Option[UsagePlanVisibility] = name match {
    case "Public"  => Public.some
    case "Private" => Private.some
    case _         => None
  }
}

sealed trait SubscriptionProcess {
  def name: String
}

object SubscriptionProcess {
  case object Automatic extends SubscriptionProcess {
    def name: String = "Automatic"
  }
  case object Manual extends SubscriptionProcess {
    def name: String = "Manual"
  }
  val values: Seq[SubscriptionProcess] = Seq(Automatic, Manual)
  def apply(name: String): Option[SubscriptionProcess] = name match {
    case "Automatic" => Automatic.some
    case "Manual"    => Manual.some
    case _           => None
  }
}

sealed trait IntegrationProcess {
  def name: String
}

object IntegrationProcess {
  case object Automatic extends IntegrationProcess {
    def name: String = "Automatic"
  }
  case object ApiKey extends IntegrationProcess {
    def name: String = "ApiKey"
  }
  val values: Seq[IntegrationProcess] = Seq(Automatic, ApiKey)
  def apply(name: String): Option[IntegrationProcess] = name match {
    case "Automatic" => Automatic.some
    case "ApiKey"    => ApiKey.some
    case _           => None
  }
}

case class Currency(code: String) extends CanJson[Currency] {
  def asJson: JsValue = json.CurrencyFormat.writes(this)
}

sealed trait UsagePlan {
  def id: UsagePlanId
  def costPerMonth: BigDecimal
  def maxRequestPerSecond: Option[Long]
  def maxRequestPerDay: Option[Long]
  def maxRequestPerMonth: Option[Long]
  def allowMultipleKeys: Option[Boolean]
  def autoRotation: Option[Boolean]
  def costFor(requests: Long): BigDecimal
  def currency: Currency
  def customName: Option[String]
  def customDescription: Option[String]
  def otoroshiTarget: Option[OtoroshiTarget]
  def trialPeriod: Option[BillingDuration]
  def billingDuration: BillingDuration
  def typeName: String
  def visibility: UsagePlanVisibility
  def authorizedTeams: Seq[TeamId]
  def asJson: JsValue = UsagePlanFormat.writes(this)
  def addAutorizedTeam(teamId: TeamId): UsagePlan
  def addAutorizedTeams(teamIds: Seq[TeamId]): UsagePlan
  def removeAuthorizedTeam(teamId: TeamId): UsagePlan
  def removeAllAuthorizedTeams(): UsagePlan
  def subscriptionProcess: SubscriptionProcess
  def integrationProcess: IntegrationProcess
  def aggregationApiKeysSecurity: Option[Boolean]
}

case object UsagePlan {
  case class Admin(
                    id: UsagePlanId,
                    customName: Option[String] = Some("Administration plan"),
                    customDescription: Option[String] = Some("access to admin api"),
                    otoroshiTarget: Option[OtoroshiTarget],
                    aggregationApiKeysSecurity: Option[Boolean] = Some(false),
                    override val authorizedTeams: Seq[TeamId] = Seq.empty
                  ) extends UsagePlan {
    override def costPerMonth: BigDecimal = BigDecimal(0)
    override def maxRequestPerSecond: Option[Long] = None
    override def maxRequestPerDay: Option[Long] = None
    override def maxRequestPerMonth: Option[Long] = None
    override def allowMultipleKeys: Option[Boolean] = Some(true)
    override def autoRotation: Option[Boolean] = Some(false)
    override def costFor(requests: Long): BigDecimal = BigDecimal(0)
    override def currency: Currency = Currency(code = "Eur")
    override def trialPeriod: Option[BillingDuration] = None
    override def billingDuration: BillingDuration =
      BillingDuration(1, BillingTimeUnit.Year)
    override def typeName: String = "Admin"
    override def visibility: UsagePlanVisibility = UsagePlanVisibility.Private
    override def addAutorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams :+ teamId)
    override def removeAuthorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams.filter(up => up != teamId))
    override def removeAllAuthorizedTeams(): UsagePlan =
      this.copy(authorizedTeams = Seq.empty)
    override def addAutorizedTeams(teamIds: Seq[TeamId]): UsagePlan =
      this.copy(authorizedTeams = teamIds)
    override def subscriptionProcess: SubscriptionProcess =
      SubscriptionProcess.Automatic
    override def integrationProcess: IntegrationProcess =
      IntegrationProcess.ApiKey
  }
  case class FreeWithoutQuotas(
                                id: UsagePlanId,
                                currency: Currency,
                                billingDuration: BillingDuration,
                                customName: Option[String],
                                customDescription: Option[String],
                                otoroshiTarget: Option[OtoroshiTarget],
                                allowMultipleKeys: Option[Boolean],
                                autoRotation: Option[Boolean],
                                subscriptionProcess: SubscriptionProcess,
                                integrationProcess: IntegrationProcess,
                                aggregationApiKeysSecurity: Option[Boolean] = Some(false),
                                override val visibility: UsagePlanVisibility = UsagePlanVisibility.Public,
                                override val authorizedTeams: Seq[TeamId] = Seq.empty
                              ) extends UsagePlan {
    override def typeName: String = "FreeWithoutQuotas"
    override def costPerMonth: BigDecimal = BigDecimal(0)
    override def maxRequestPerSecond: Option[Long] = None
    override def maxRequestPerDay: Option[Long] = None
    override def maxRequestPerMonth: Option[Long] = None
    override def costFor(requests: Long): BigDecimal = BigDecimal(0)
    override def trialPeriod: Option[BillingDuration] = None
    override def addAutorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams :+ teamId)
    override def removeAuthorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams.filter(up => up != teamId))
    override def removeAllAuthorizedTeams(): UsagePlan =
      this.copy(authorizedTeams = Seq.empty)
    override def addAutorizedTeams(teamIds: Seq[TeamId]): UsagePlan =
      this.copy(authorizedTeams = teamIds)
  }
  case class FreeWithQuotas(
                             id: UsagePlanId,
                             maxPerSecond: Long,
                             maxPerDay: Long,
                             maxPerMonth: Long,
                             currency: Currency,
                             billingDuration: BillingDuration,
                             customName: Option[String],
                             customDescription: Option[String],
                             otoroshiTarget: Option[OtoroshiTarget],
                             allowMultipleKeys: Option[Boolean],
                             autoRotation: Option[Boolean],
                             subscriptionProcess: SubscriptionProcess,
                             integrationProcess: IntegrationProcess,
                             aggregationApiKeysSecurity: Option[Boolean] = Some(false),
                             override val visibility: UsagePlanVisibility = UsagePlanVisibility.Public,
                             override val authorizedTeams: Seq[TeamId] = Seq.empty
                           ) extends UsagePlan {
    override def typeName: String = "FreeWithQuotas"
    override def costPerMonth: BigDecimal = BigDecimal(0)
    override def maxRequestPerSecond: Option[Long] = maxPerSecond.some
    override def maxRequestPerDay: Option[Long] = maxPerDay.some
    override def maxRequestPerMonth: Option[Long] = maxPerMonth.some
    override def costFor(requests: Long): BigDecimal = BigDecimal(0)
    override def trialPeriod: Option[BillingDuration] = None
    override def addAutorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams :+ teamId)
    override def removeAuthorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams.filter(up => up != teamId))
    override def removeAllAuthorizedTeams(): UsagePlan =
      this.copy(authorizedTeams = Seq.empty)
    override def addAutorizedTeams(teamIds: Seq[TeamId]): UsagePlan =
      this.copy(authorizedTeams = teamIds)
  }
  case class QuotasWithLimits(
                               id: UsagePlanId,
                               maxPerSecond: Long,
                               maxPerDay: Long,
                               maxPerMonth: Long,
                               costPerMonth: BigDecimal,
                               trialPeriod: Option[BillingDuration],
                               currency: Currency,
                               billingDuration: BillingDuration,
                               customName: Option[String],
                               customDescription: Option[String],
                               otoroshiTarget: Option[OtoroshiTarget],
                               allowMultipleKeys: Option[Boolean],
                               autoRotation: Option[Boolean],
                               subscriptionProcess: SubscriptionProcess,
                               integrationProcess: IntegrationProcess,
                               aggregationApiKeysSecurity: Option[Boolean] = Some(false),
                               override val visibility: UsagePlanVisibility = UsagePlanVisibility.Public,
                               override val authorizedTeams: Seq[TeamId] = Seq.empty
                             ) extends UsagePlan {
    override def typeName: String = "QuotasWithLimits"
    override def maxRequestPerSecond: Option[Long] = maxPerSecond.some
    override def maxRequestPerDay: Option[Long] = maxPerDay.some
    override def maxRequestPerMonth: Option[Long] = maxPerMonth.some
    override def costFor(requests: Long): BigDecimal = costPerMonth
    override def addAutorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams :+ teamId)
    override def removeAuthorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams.filter(up => up != teamId))
    override def removeAllAuthorizedTeams(): UsagePlan =
      this.copy(authorizedTeams = Seq.empty)
    override def addAutorizedTeams(teamIds: Seq[TeamId]): UsagePlan =
      this.copy(authorizedTeams = teamIds)
  }
  case class QuotasWithoutLimits(
                                  id: UsagePlanId,
                                  maxPerSecond: Long,
                                  maxPerDay: Long,
                                  maxPerMonth: Long,
                                  costPerAdditionalRequest: BigDecimal,
                                  costPerMonth: BigDecimal,
                                  trialPeriod: Option[BillingDuration],
                                  currency: Currency,
                                  billingDuration: BillingDuration,
                                  customName: Option[String],
                                  customDescription: Option[String],
                                  otoroshiTarget: Option[OtoroshiTarget],
                                  allowMultipleKeys: Option[Boolean],
                                  autoRotation: Option[Boolean],
                                  subscriptionProcess: SubscriptionProcess,
                                  integrationProcess: IntegrationProcess,
                                  aggregationApiKeysSecurity: Option[Boolean] = Some(false),
                                  override val visibility: UsagePlanVisibility = UsagePlanVisibility.Public,
                                  override val authorizedTeams: Seq[TeamId] = Seq.empty
                                ) extends UsagePlan {
    override def typeName: String = "QuotasWithoutLimits"
    override def maxRequestPerSecond: Option[Long] = maxPerSecond.some
    override def maxRequestPerDay: Option[Long] = maxPerDay.some
    override def maxRequestPerMonth: Option[Long] = maxPerMonth.some
    override def costFor(requests: Long): BigDecimal =
      costPerMonth + (Math.max(requests - maxPerMonth, 0) * costPerAdditionalRequest)
    override def addAutorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams :+ teamId)
    override def removeAuthorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams.filter(up => up != teamId))
    override def removeAllAuthorizedTeams(): UsagePlan =
      this.copy(authorizedTeams = Seq.empty)
    override def addAutorizedTeams(teamIds: Seq[TeamId]): UsagePlan =
      this.copy(authorizedTeams = teamIds)
  }
  case class PayPerUse(
                        id: UsagePlanId,
                        costPerMonth: BigDecimal,
                        costPerRequest: BigDecimal,
                        trialPeriod: Option[BillingDuration],
                        currency: Currency,
                        billingDuration: BillingDuration,
                        customName: Option[String],
                        customDescription: Option[String],
                        otoroshiTarget: Option[OtoroshiTarget],
                        allowMultipleKeys: Option[Boolean],
                        autoRotation: Option[Boolean],
                        subscriptionProcess: SubscriptionProcess,
                        integrationProcess: IntegrationProcess,
                        aggregationApiKeysSecurity: Option[Boolean] = Some(false),
                        override val visibility: UsagePlanVisibility = UsagePlanVisibility.Public,
                        override val authorizedTeams: Seq[TeamId] = Seq.empty
                      ) extends UsagePlan {
    override def typeName: String = "PayPerUse"
    override def costFor(requests: Long): BigDecimal =
      costPerMonth + (requests * costPerRequest)
    override def maxRequestPerMonth: Option[Long] = None
    override def maxRequestPerSecond: Option[Long] = None
    override def maxRequestPerDay: Option[Long] = None
    override def addAutorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams :+ teamId)
    override def removeAuthorizedTeam(teamId: TeamId): UsagePlan =
      this.copy(authorizedTeams = authorizedTeams.filter(up => up != teamId))
    override def removeAllAuthorizedTeams(): UsagePlan =
      this.copy(authorizedTeams = Seq.empty)
    override def addAutorizedTeams(teamIds: Seq[TeamId]): UsagePlan =
      this.copy(authorizedTeams = teamIds)
  }
}

case class OtoroshiApiKey(
                           clientName: String,
                           clientId: String,
                           clientSecret: String
                         ) extends CanJson[OtoroshiApiKey] {
  override def asJson: JsValue = json.OtoroshiApiKeyFormat.writes(this)
}

case class SwaggerAccess(url: String,
                         content: Option[String] = None,
                         headers: Map[String, String] =
                         Map.empty[String, String]) {
  def swaggerContent()(implicit ec: ExecutionContext,
                       env: Env): Future[JsValue] = {
    content match {
      case Some(c) => FastFuture.successful(Json.parse(c))
      case None => {
        val finalUrl =
          if (url.startsWith("/")) s"http://127.0.0.1:${env.config.port}${url}"
          else url
        env.wsClient
          .url(finalUrl)
          .withHttpHeaders(headers.toSeq: _*)
          .get()
          .map { resp =>
            Json.parse(resp.body)
          }
      }
    }
  }
}

case class ApiDocumentation(id: ApiDocumentationId,
                            tenant: TenantId,
                            pages: Seq[ApiDocumentationPageId],
                            lastModificationAt: DateTime)
  extends CanJson[ApiDocumentation] {
  override def asJson: JsValue = json.ApiDocumentationFormat.writes(this)
  def fetchPages(tenant: Tenant)(implicit ec: ExecutionContext, env: Env) = {
    env.dataStore.apiDocumentationPageRepo
      .forTenant(tenant.id)
      .findWithProjection(
        Json.obj(
          "_deleted" -> false,
          "_id" -> Json.obj(
            "$in" -> JsArray(pages.map(_.value).map(JsString.apply).toSeq))),
        Json.obj(
          "_id" -> true,
          "_humanReadableId" -> true,
          "title" -> true,
          "level" -> true,
          "lastModificationAt" -> true,
          "content" -> true,
          "contentType" -> true
        )
      )
      .map { list =>
        // TODO: fetch remote content
        pages
          .map(id => list.find(o => (o \ "_id").as[String] == id.value))
          .collect { case Some(e) => e }
      }
  }
}

// "https://mozilla.github.io/pdf.js/web/compressed.tracemonkey-pldi-09.pdf"
case class ApiDocumentationPage(id: ApiDocumentationPageId,
                                tenant: TenantId,
                                deleted: Boolean = false,
                                // api: ApiId,
                                title: String,
                                //index: Double,
                                level: Int = 0,
                                lastModificationAt: DateTime,
                                content: String,
                                contentType: String = "text/markdown",
                                remoteContentEnabled: Boolean = false,
                                remoteContentUrl: Option[String] = None,
                                remoteContentHeaders: Map[String, String] =
                                Map.empty[String, String])
  extends CanJson[ApiDocumentationPage] {
  //def humanReadableId = s"$index-$level-${title.urlPathSegmentSanitized}"
  def humanReadableId = s"$level-${title.urlPathSegmentSanitized}"
  override def asJson: JsValue = json.ApiDocumentationPageFormat.writes(this)
  def asWebUiJson: JsValue =
    json.ApiDocumentationPageFormat.writes(this).as[JsObject]
}

case class ApiPost(id: ApiPostId,
                   tenant: TenantId,
                   deleted: Boolean = false,
                   title: String,
                   lastModificationAt: DateTime,
                   content: String)
  extends CanJson[ApiPost] {
  def humanReadableId: String = title.urlPathSegmentSanitized
  override def asJson: JsValue = json.ApiPostFormat.writes(this)
}

case class ApiIssueTag(id: ApiIssueTagId, name: String, color: String)

case class ApiIssueComment(by: UserId,
                           createdAt: DateTime,
                           lastModificationAt: DateTime,
                           content: String)

case class ApiIssue(id: ApiIssueId,
                    seqId: Int,
                    tenant: TenantId,
                    deleted: Boolean = false,
                    title: String,
                    tags: Set[ApiIssueTagId],
                    open: Boolean,
                    createdAt: DateTime,
                    closedAt: Option[DateTime],
                    by: UserId,
                    comments: Seq[ApiIssueComment],
                    lastModificationAt: DateTime,
                    apiVersion: Option[String] = Some("1.0.0"))
  extends CanJson[ApiIssue] {
  def humanReadableId: String = seqId.toString
  override def asJson: JsValue = json.ApiIssueFormat.writes(this)
}

sealed trait TestingAuth {
  def name: String
}

object TestingAuth {
  case object ApiKey extends TestingAuth {
    def name: String = "ApiKey"
  }
  case object Basic extends TestingAuth {
    def name: String = "Basic"
  }
}

case class TestingConfig(
                          otoroshiSettings: OtoroshiSettingsId,
                          authorizedEntities: AuthorizedEntities,
                          clientName: String,
                          api: ApiId,
                          tag: String,
                          customMetadata: Option[JsObject],
                          customMaxPerSecond: Option[Long],
                          customMaxPerDay: Option[Long],
                          customMaxPerMonth: Option[Long],
                          customReadOnly: Option[Boolean]
                        ) extends CanJson[TestingConfig] {
  override def asJson: JsValue = json.TestingConfigFormat.writes(this)
}

case class Testing(
                    enabled: Boolean = false,
                    auth: TestingAuth = TestingAuth.Basic,
                    name: Option[String] = None,
                    username: Option[String] = None,
                    password: Option[String] = None,
                    config: Option[TestingConfig] = None,
                  ) extends CanJson[Testing] {
  override def asJson: JsValue = json.TestingFormat.writes(this)
}

case class Api(
                id: ApiId,
                tenant: TenantId,
                deleted: Boolean = false,
                team: TeamId,
                name: String,
                smallDescription: String,
                header: Option[String] = None,
                image: Option[String] = None,
                description: String,
                currentVersion: Version = Version("1.0.0"),
                supportedVersions: Set[Version] = Set(Version("1.0.0")),
                isDefault: Boolean = false,
                lastUpdate: DateTime,
                published: Boolean = false,
                testing: Testing = Testing(),
                documentation: ApiDocumentation,
                swagger: Option[SwaggerAccess] = Some(
                  SwaggerAccess(url = "/assets/swaggers/petstore.json")),
                tags: Set[String] = Set.empty,
                categories: Set[String] = Set.empty,
                visibility: ApiVisibility,
                possibleUsagePlans: Seq[UsagePlan],
                defaultUsagePlan: UsagePlanId,
                authorizedTeams: Seq[TeamId] = Seq.empty,
                posts: Seq[ApiPostId] = Seq.empty,
                issues: Seq[ApiIssueId] = Seq.empty,
                issuesTags: Set[ApiIssueTag] = Set.empty,
                stars: Int = 0,
                parent: Option[ApiId] = None,
                apis: Option[Set[ApiId]] = None
              ) extends CanJson[User] {
  def humanReadableId = name.urlPathSegmentSanitized
  override def asJson: JsValue = json.ApiFormat.writes(this)
  def asSimpleJson: JsValue = Json.obj(
    "_id" -> id.asJson,
    "_humanReadableId" -> name.urlPathSegmentSanitized,
    "_tenant" -> tenant.asJson,
    "team" -> team.value,
    "name" -> name,
    "smallDescription" -> smallDescription,
    "header" -> header.map(JsString).getOrElse(JsNull).as[JsValue],
    "image" -> image.map(JsString).getOrElse(JsNull).as[JsValue],
    "description" -> description,
    "currentVersion" -> currentVersion.asJson,
    "supportedVersions" -> JsArray(supportedVersions.map(_.asJson).toSeq),
    "tags" -> JsArray(tags.map(JsString.apply).toSeq),
    "categories" -> JsArray(categories.map(JsString.apply).toSeq),
    "visibility" -> visibility.name,
    "possibleUsagePlans" -> JsArray(possibleUsagePlans.map(_.asJson).toSeq),
    "posts" -> SeqPostIdFormat.writes(posts),
    "issues" -> SeqIssueIdFormat.writes(issues),
    "issuesTags" -> SetApiTagFormat.writes(issuesTags),
    "stars" -> stars,
    "parent" -> parent.map(_.asJson).getOrElse(JsNull).as[JsValue],
    "isDefault" -> isDefault
  )
  def asIntegrationJson(teams: Seq[Team]): JsValue = {
    val t = teams.find(_.id == team).get.name.urlPathSegmentSanitized
    Json.obj(
      "id" -> s"${t}/${name.urlPathSegmentSanitized}",
      "team" -> t,
      "name" -> name,
      "smallDescription" -> smallDescription,
      "currentVersion" -> currentVersion.asJson,
      "supportedVersions" -> JsArray(supportedVersions.map(_.asJson).toSeq),
      "tags" -> JsArray(tags.map(JsString.apply).toSeq),
      "categories" -> JsArray(categories.map(JsString.apply).toSeq),
      "visibility" -> visibility.name,
      "stars" -> stars
    )
  }
  def asPublicWithAuthorizationsJson(): JsValue = Json.obj(
    "_id" -> id.value,
    "_humanReadableId" -> name.urlPathSegmentSanitized,
    "tenant" -> tenant.asJson,
    "team" -> team.value,
    "name" -> name,
    "smallDescription" -> smallDescription,
    "description" -> description,
    "currentVersion" -> currentVersion.asJson,
    "isDefault" -> isDefault,
    "published" -> published,
    "tags" -> JsArray(tags.map(JsString.apply).toSeq),
    "categories" -> JsArray(categories.map(JsString.apply).toSeq),
    "authorizedTeams" -> SeqTeamIdFormat.writes(authorizedTeams),
    "stars" -> stars,
    "parent" -> parent.map(_.asJson).getOrElse(JsNull).as[JsValue]
  )
}

case class AuthorizedEntities(services: Set[OtoroshiServiceId] = Set.empty,
                              groups: Set[OtoroshiServiceGroupId] = Set.empty)
  extends CanJson[AuthorizedEntities] {
  def asJson: JsValue = json.AuthorizedEntitiesFormat.writes(this)
  def asOtoroshiJson: JsValue =
    json.AuthorizedEntitiesOtoroshiFormat.writes(this)
  def isEmpty: Boolean = services.isEmpty && groups.isEmpty
  def equalsAuthorizedEntities(a: AuthorizedEntities): Boolean =
    services.forall(s => a.services.contains(s)) && groups.forall(g =>
      a.groups.contains(g))
}

case class ApiWithAuthorizations(api: Api, authorizations: Seq[AuthorizationApi] = Seq.empty)

case class AuthorizationApi(team: String, authorized: Boolean, pending: Boolean)

case class ApiGroup(
                     id: ApiGroupId,
                     tenant: TenantId,
                     deleted: Boolean = false,
                     team: TeamId,
                     name: String,
                     smallDescription: String,
                     description: String,
                     published: Boolean = false,
                     apis: Set[ApiId],
                     visibility: ApiVisibility,
                     tags: Set[String] = Set.empty,
                     categories: Set[String] = Set.empty,
                     possibleUsagePlans: Seq[UsagePlan],
                     defaultUsagePlan: UsagePlanId,
                     authorizedTeams: Seq[TeamId] = Seq.empty,
                     posts: Seq[ApiPostId] = Seq.empty,
                     issues: Seq[ApiIssueId] = Seq.empty,
                     issuesTags: Set[ApiIssueTag] = Set.empty,
                     stars: Int = 0,
                   ) extends CanJson[ApiGroup] {
  override def asJson: JsValue = json.ApiGroupFormat.writes(this)
}