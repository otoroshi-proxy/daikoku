package fr.maif.otoroshi.daikoku.ctrls

import com.github.blemale.scaffeine.{Cache, Scaffeine}
import com.nimbusds.jose.util.StandardCharset
import controllers.Assets
import daikoku.BuildInfo
import fr.maif.otoroshi.daikoku.actions.{
  DaikokuAction,
  DaikokuActionMaybeWithGuest,
  DaikokuActionMaybeWithoutUser,
  DaikokuActionMaybeWithoutUserContext
}
import fr.maif.otoroshi.daikoku.audit.AuditTrailEvent
import fr.maif.otoroshi.daikoku.ctrls.authorizations.async.{
  DaikokuAdminOrSelf,
  TenantAdminOnly
}
import fr.maif.otoroshi.daikoku.ctrls.authorizations.sync.TeamMemberOnly
import fr.maif.otoroshi.daikoku.domain._
import fr.maif.otoroshi.daikoku.domain.json.{
  CmsFileFormat,
  CmsPageFormat,
  CmsRequestRenderingFormat
}
import fr.maif.otoroshi.daikoku.env.Env
import fr.maif.otoroshi.daikoku.logger.AppLogger
import fr.maif.otoroshi.daikoku.utils.Errors
import org.apache.pekko.http.scaladsl.util.FastFuture
import org.joda.time.DateTime
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json._
import play.api.mvc._

import java.io.{ByteArrayOutputStream, File, FileInputStream, FileOutputStream}
import java.util
import java.util.concurrent.TimeUnit
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

class HomeController(
    DaikokuActionMaybeWithoutUser: DaikokuActionMaybeWithoutUser,
    DaikokuActionMaybeWithGuest: DaikokuActionMaybeWithGuest,
    DaikokuAction: DaikokuAction,
    env: Env,
    cc: ControllerComponents,
    assets: Assets
) extends AbstractController(cc)
    with I18nSupport {

  implicit val ec: ExecutionContext = env.defaultExecutionContext
  implicit val e: Env = env
  implicit val m: MessagesApi = messagesApi

  case class CmsPageCache(contentType: String, content: String)

  private val cache: Cache[String, CmsPageCache] = Scaffeine()
    .expireAfterWrite(60.seconds)
    .maximumSize(100)
    .build[String, CmsPageCache]()

  private def manageCmsHome[A](
      ctx: DaikokuActionMaybeWithoutUserContext[A],
      redirectTo: Result
  ) = {
    ctx.tenant.style match {
      case Some(value) if value.homePageVisible =>
        value.homeCmsPage match {
          case Some(pageId) =>
            if (!ctx.tenant.isPrivate || ctx.user.exists(!_.isGuest))
              cmsPageByIdWithoutAction(ctx, pageId)
          case _ =>
            AppLogger.warn("tenant is private (3)")
            FastFuture.successful(redirectTo)
        }
      case _ => FastFuture.successful(redirectTo)
    }
  }

  def index() =
    DaikokuActionMaybeWithoutUser.async { ctx =>
      assets.at("index.html").apply(ctx.request)
    }

  def indexForRobots() =
    DaikokuActionMaybeWithoutUser.async { ctx =>
      ctx.tenant.robotTxt match {
        case Some(robotTxt) =>
          FastFuture.successful(Ok(views.txt.robot.render(robotTxt)))
        case None =>
          FastFuture.successful(
            NotFound(Json.obj("error" -> "robots.txt not found"))
          )
      }
    }

  def indexWithPath(path: String) =
    DaikokuActionMaybeWithoutUser.async { ctx =>
      assets.at("index.html").apply(ctx.request)
    }

  def health() =
    DaikokuActionMaybeWithGuest { ctx =>
      ctx.request.headers.get("Otoroshi-Health-Check-Logic-Test") match {
        //todo: better health check
        case Some(value) =>
          Ok.withHeaders(
            "Otoroshi-Health-Check-Logic-Test-Result" -> (value.toLong + 42L).toString
          )
        case None =>
          Ok(
            Json.obj(
              "tenantMode" -> ctx.tenant.tenantMode
                .getOrElse(TenantMode.Default)
                .name
            )
          )
      }
    }

  def getDaikokuVersion() =
    DaikokuActionMaybeWithoutUser { ctx =>
      Ok(Json.obj("version" -> BuildInfo.version))
    }

  def getConnectedUser() = {
    DaikokuActionMaybeWithoutUser { ctx =>
      Ok(
        Json.obj(
          "connectedUser" -> ctx.user
            .map(_.toUiPayload())
            .getOrElse(JsNull)
            .as[JsValue],
          "impersonator" -> ctx.session
            .map(_.impersonatorJson())
            .getOrElse(JsNull)
            .as[JsValue],
          "session" -> ctx.session
            .map(_.asSimpleJson)
            .getOrElse(JsNull)
            .as[JsValue],
          "tenant" -> ctx.tenant.toUiPayload(env),
          "isTenantAdmin" -> ctx.isTenantAdmin,
          "apiCreationPermitted" -> ctx.apiCreationPermitted,
          "loginAction" -> fr.maif.otoroshi.daikoku.ctrls.routes.LoginController
            .login(ctx.tenant.authProvider.name)
            .url
        )
      )
    }
  }

  private def getMatchingRoutes(
      path: String,
      cmsPaths: Seq[(String, CmsPage)],
      strictMode: Boolean = false
  ): Seq[CmsPage] = {
    val paths = path
      .replace("/_", "")
      .split("/")
      .filter(_.nonEmpty)

    if (paths.isEmpty)
      Seq()
    else {
      var matched = false

      val init: Seq[(Array[String], CmsPage)] = cmsPaths
        .map(r =>
          (
            r._1.replace("/_/", "").split("/") ++ Array(
              if (r._2.exact) "" else "*"
            ),
            r._2
          )
        )
        .map(p => (p._1.filter(_.nonEmpty), p._2))
        .filter(p => p._1.nonEmpty);

      paths
        .foldLeft(
          init
        ) { (paths, path) =>
          {
            if (paths.isEmpty || matched)
              paths
            else {
              val matchingRoutes: Seq[(Array[String], CmsPage)] = paths.filter(
                p => p._1.nonEmpty && (p._1.head == path || p._1.head == "*")
              )
              if (matchingRoutes.nonEmpty)
                matchingRoutes.map(p => (p._1.tail, p._2))
              else {
                val matchingRoute = paths.find(p => p._1.isEmpty)
                if (matchingRoute.nonEmpty && !strictMode) {
                  matched = true
                  Seq(matchingRoute.get)
                } else
                  Seq()
              }
            }
          }
        }
        .map(_._2)
    }
  }

  def renderCmsPageFromBody(path: String) =
    DaikokuActionMaybeWithoutUser.async(parse.json) { ctx =>
      val req = ctx.request.body.as[JsObject].as(CmsRequestRenderingFormat)

      val currentPage = req.content.find(_.path() == req.current_page)

      currentPage match {
        case Some(r)
            if r.authenticated() && (ctx.user.isEmpty || ctx.user.exists(
              _.isGuest
            )) =>
          redirectToLoginPage(ctx)
        case Some(r) if !r.visible() => cmsPageNotFound(ctx)
        case Some(page)              => render(ctx, page.toCmsPage(ctx.tenant.id), Some(req))
        case None                    => cmsPageNotFound(ctx)
      }
    }

  private def renderCmsPage[A](
      ctx: DaikokuActionMaybeWithoutUserContext[A],
      page: Option[CmsPage]
  ) = {
    page match {
      case Some(r)
          if r.authenticated && (ctx.user.isEmpty || ctx.user
            .exists(_.isGuest)) =>
        redirectToLoginPage(ctx)
      case Some(r) => render(ctx, r)
      case None    => cmsPageNotFound(ctx)
    }
  }

  def cmsPageByPath(path: String, page: Option[CmsPage] = None) =
    DaikokuActionMaybeWithoutUser.async {
      ctx: DaikokuActionMaybeWithoutUserContext[AnyContent] =>
        val actualPath = if (path.startsWith("/")) {
          path
        } else {
          s"/$path"
        }

        if (
          ctx.request
            .getQueryString("draft")
            .contains("true") && !ctx.isTenantAdmin && !ctx.user.exists(
            _.isDaikokuAdmin
          )
        ) {
          Errors.craftResponseResult(
            "User not found :-(",
            Results.NotFound,
            ctx.request,
            None,
            env
          )
        } else {
          env.dataStore.cmsRepo
            .forTenant(ctx.tenant)
            .findOneNotDeleted(Json.obj("path" -> actualPath))
            .flatMap {
              case None =>
                env.dataStore.cmsRepo
                  .forTenant(ctx.tenant)
                  .findAllNotDeleted()
                  .map(cmsPages =>
                    cmsPages.filter(p => p.path.exists(_.nonEmpty))
                  )
                  .flatMap(cmsPages => {
                    val strictPage =
                      getMatchingRoutes(
                        ctx.request.path,
                        cmsPages
                          .filter(p => p.exact && p.path.nonEmpty)
                          .map(p => (p.path.get, p)),
                        true
                      )

                    val page =
                      if (strictPage.nonEmpty)
                        strictPage
                      else
                        getMatchingRoutes(
                          ctx.request.path,
                          cmsPages
                            .filter(p => !p.exact && p.path.nonEmpty)
                            .map(p => (p.path.get, p))
                        )

                    renderCmsPage(ctx, page.headOption)
                  })
              case Some(page) if !page.visible => cmsPageNotFound(ctx)
              case Some(page) if page.authenticated && ctx.user.isEmpty =>
                redirectToLoginPage(ctx)
              case Some(page) => render(ctx, page)
            }
        }
    }

  private def redirectToLoginPage[A](
      ctx: DaikokuActionMaybeWithoutUserContext[A]
  ) =
    FastFuture.successful(
      Redirect(
        s"/auth/${ctx.tenant.authProvider.name}/login?redirect=${ctx.request.path}"
      )
    )

  private def cmsPageNotFound[A](
      ctx: DaikokuActionMaybeWithoutUserContext[A]
  ): Future[Result] = {
    val optionFoundPage: Option[DaikokuStyle] = ctx.tenant.style
      .find(p => p.homePageVisible && p.notFoundCmsPage.nonEmpty)

    optionFoundPage match {
      case Some(p) =>
        env.dataStore.cmsRepo
          .forTenant(ctx.tenant)
          .findById(p.notFoundCmsPage.get)
          .flatMap {
            case Some(page) =>
              page.render(ctx, req = None).map(res => Ok(res._1).as(res._2))
            case _ =>
              Errors.craftResponseResult(
                "Page not found !",
                Results.NotFound,
                ctx.request,
                None,
                env
              )
          }
      case _ =>
        Errors.craftResponseResult(
          "Page not found !",
          Results.NotFound,
          ctx.request,
          None,
          env
        )
    }
  }

  private def render[A](
      ctx: DaikokuActionMaybeWithoutUserContext[A],
      r: CmsPage,
      req: Option[CmsRequestRendering] = None,
      skipCache: Boolean = false,
      fields: Map[String, JsValue] = Map.empty[String, JsValue]
  ) = {

    val isDraftRender: Boolean =
      ctx.request.getQueryString("draft").contains("true")
    val forceReloading: Boolean =
      ctx.request.getQueryString("force_reloading").contains("true") || skipCache

    val cacheId =
      s"${ctx.user.map(_.id.value).getOrElse("")}-${r.path.getOrElse("")}"

    cache
      .policy()
      .expireAfterWrite()
      .ifPresent(eviction => {
        val ttl: Long = ctx.tenant.style
          .map(_.cacheTTL)
          .getOrElse(60000)
          .asInstanceOf[Number]
          .longValue
        if (eviction.getExpiresAfter(TimeUnit.MILLISECONDS) != ttl) {
          cache.invalidateAll()
          eviction.setExpiresAfter(ttl, TimeUnit.MILLISECONDS)
        }
      })

    if (isDraftRender || forceReloading)
      r.render(ctx, None, req = req, jsonToCombine = fields).map(res => Ok(res._1).as(res._2))
    else
      cache.getIfPresent(cacheId) match {
        case Some(value) =>
          FastFuture.successful(Ok(value.content).as(value.contentType))
        case _ =>
          r.render(ctx, None, req = req, jsonToCombine = fields)
            .map(res => {
              cache.put(
                cacheId,
                CmsPageCache(content = res._1, contentType = res._2)
              )
              Ok(res._1).as(res._2)
            })
      }
  }

  private def cmsPageByIdWithoutAction[A](
      ctx: DaikokuActionMaybeWithoutUserContext[A],
      id: String,
      skipCache: Boolean = false,
      fields: Map[String, JsValue] = Map.empty
  ) = {
    env.dataStore.cmsRepo.forTenant(ctx.tenant).findByIdNotDeleted(id).flatMap {
      case None                        => cmsPageNotFound(ctx)
      case Some(page) if !page.visible => cmsPageNotFound(ctx)
      case Some(page) if page.authenticated && ctx.user.isEmpty =>
        FastFuture.successful(
          Redirect(
            s"/auth/${ctx.tenant.authProvider.name}/login?redirect=${ctx.request.path}"
          )
        )
      case Some(page) =>
        render(ctx, page, skipCache = skipCache, fields = fields)
    }
  }

  def cmsPageById(id: String) =
    DaikokuActionMaybeWithoutUser.async { ctx =>
      cmsPageByIdWithoutAction(ctx, id, skipCache = true)
    }

  def advancedRenderCmsPageById(id: String) =
    DaikokuActionMaybeWithoutUser.async(parse.json) { ctx =>
      cmsPageByIdWithoutAction(ctx, id,
        skipCache = true,
        fields = ctx.request.body
          .asOpt[JsObject]
          .flatMap(body => (body \ "fields").asOpt[Map[String, JsValue]])
          .getOrElse(Map.empty[String, JsValue]))
    }

  def getCmsPage(id: String) =
    DaikokuAction.async { ctx =>
      TenantAdminOnly(
        AuditTrailEvent("@{user.name} get the cms page @{pageName}")
      )(ctx.tenant.id.value, ctx) { (tenant, _) =>
        {
          env.dataStore.cmsRepo
            .forTenant(tenant)
            .findById(id)
            .map {
              case None       => NotFound(Json.obj("error" -> "cms page not found"))
              case Some(page) => Ok(page.asJson)
            }
        }
      }
    }

//  def session(userId: String) =
//    DaikokuAction.async { ctx =>
//      DaikokuAdminOrSelf(AuditTrailEvent("@{user.name} get session"))(
//        UserId(userId),
//        ctx
//      ) {
//        val token =
//          ctx.request.cookies.get("daikoku-session").map(_.value).getOrElse("")
//        FastFuture.successful(Ok(Json.obj("token" -> token)))
//      }
//    }

  def deleteCmsPage(id: String) =
    DaikokuAction.async { ctx =>
      TenantAdminOnly(AuditTrailEvent("@{user.name} has removed a cms page"))(
        ctx.tenant.id.value,
        ctx
      ) { (tenant, _) =>
        env.dataStore.cmsRepo
          .forTenant(tenant)
          .deleteByIdLogically(id)
          .map {
            case true => Ok(Json.obj("created" -> true))
            case false =>
              BadRequest(Json.obj("error" -> "Unable to remove the cms page"))
          }
      }
    }

  private val contentTypeToExtension = Map(
    "application/json" -> "json",
    "text/html" -> "html",
    "text/javascript" -> "js",
    "text/css" -> "css",
    "text/markdown" -> "md",
    "text/plain" -> "txt",
    "text/xml" -> "xml"
  )

//  def summary() =
//    DaikokuAction.async { ctx =>
//      TenantAdminOnly(
//        AuditTrailEvent("@{user.name} has download the cms summary")
//      )(ctx.tenant.id.value, ctx) { (tenant, _) =>
//        env.dataStore.cmsRepo
//          .forTenant(tenant)
//          .findAllNotDeleted()
//          .map(pages => {
//            val summary = pages.foldLeft(Json.arr()) { (acc, page) =>
//              acc ++ Json
//                .arr(page.asJson.as[JsObject] - "draft" - "history")
//            }
//
//            Ok(summary)
//          })
//      }
//    }

  def importFromZip() =
    DaikokuAction.async(parse.multipartFormData) { ctx =>
      try {
        ctx.request.body
          .file("file") match {
          case Some(zip) =>
            val out = new ZipInputStream(new FileInputStream(zip.ref))
            var files = Map.empty[String, String]

            var zipEntry: ZipEntry = null
            while ({
              zipEntry = out.getNextEntry
              Option(zipEntry).isDefined
            }) {
              val size =
                if (zipEntry.getCompressedSize.toInt > 0)
                  zipEntry.getCompressedSize.toInt
                else 4096
              if (size > 0) {
                val outputStream: ByteArrayOutputStream =
                  new ByteArrayOutputStream()
                val buffer: Array[Byte] = Array.ofDim(size)
                var length = 0

                while ({
                  length = out.read(buffer)
                  length != -1
                }) {
                  outputStream.write(buffer, 0, length)
                }

                files = files + (zipEntry.getName -> outputStream.toString(
                  StandardCharset.UTF_8
                ))
                outputStream.close()
              }
            }
            out.close()

            if (files.isEmpty)
              FastFuture.successful(
                BadRequest(Json.obj("error" -> "the zip file is empty"))
              )
            else {
              files.find(file => file._1 == "summary.json") match {
                case None =>
                  FastFuture.successful(
                    BadRequest(Json.obj("error" -> "summary json file missing"))
                  )
                case Some((_, summaryContent)) =>
                  val jsonSummary = Json.parse(summaryContent)
                  val pages: Seq[CmsPage] = (jsonSummary \ "pages").as(
                    Format(Reads.seq(CmsPageFormat), Writes.seq(CmsPageFormat))
                  )
                  Future
                    .sequence(pages.map { page =>
                      val filename =
                        s"${page.name}.${contentTypeToExtension.getOrElse(page.contentType, ".txt")}"
                      val optFile = files.find(f => f._1 == filename)
                      val content = optFile match {
                        case Some((_, value)) => value
                        case None             => page.draft
                      }
                      env.dataStore.cmsRepo
                        .forTenant(ctx.tenant)
                        .save(
                          page.copy(
                            draft = content,
                            body = content,
                            tenant = ctx.tenant.id,
                          )
                        )
                    })
                    .map { _ =>
                      Ok(Json.obj("done" -> true))
                    }
              }
            }
          case _ =>
            FastFuture.successful(
              BadRequest(Json.obj("error" -> "missing zip"))
            )
        }
      } catch {
        case e: Throwable =>
          e.printStackTrace(System.out)
          FastFuture.successful(Ok(Json.obj("done" -> true)))
      }
    }
}
