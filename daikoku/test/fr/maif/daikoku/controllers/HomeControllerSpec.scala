package fr.maif.daikoku.controllers

import com.dimafeng.testcontainers.GenericContainer.FileSystemBind
import com.dimafeng.testcontainers.lifecycle.and
import com.dimafeng.testcontainers.scalatest.{
  TestContainersForAll,
  TestContainersForEach
}
import com.dimafeng.testcontainers.GenericContainer
import fr.maif.daikoku.domain.*
import fr.maif.daikoku.testUtils.DaikokuSpecHelper
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.play.PlaySpec
import org.testcontainers.containers.BindMode
import cats.implicits.catsSyntaxOptionId
import fr.maif.daikoku.BuildInfo
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

trait HomeControllerContainers {
  type Containers = GenericContainer and GenericContainer

  val pwd = System.getProperty("user.dir")

  def createContainers(): Containers = {
    val containerOto = GenericContainer
      .Def(
        dockerImage = "maif/otoroshi",
        exposedPorts = Seq(8080),
        fileSystemBind = Seq(
          FileSystemBind(
            s"$pwd/test/fr/maif/daikoku/controllers/otoroshi.json",
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

  def healthResult(
      tenantMode: TenantMode = TenantMode.Default,
      datastoreStatus: ServiceStatus = ServiceStatus.Up,
      mailerStatus: ServiceStatus = ServiceStatus.Up,
      s3Status: ServiceStatus = ServiceStatus.Absent,
      otoroshiStatus: ServiceStatus = ServiceStatus.Up,
      otoroshiPort: String
  ): JsObject = Json.obj(
    "status" -> datastoreStatus.value,
    "datastore" -> datastoreStatus.value,
    "version" -> BuildInfo.version,
    "Test Corp." -> Json.obj(
      "tenantMode" -> tenantMode.name,
      "status" -> Json.obj(
        "mailer" -> mailerStatus.value,
        "S3" -> s3Status.value,
        "otoroshi" -> Json.arr(
          Json.obj(
            s"http://otoroshi.oto.tools:$otoroshiPort (otoroshi-api.oto.tools)" -> otoroshiStatus.value
          )
        )
      )
    )
  )
}

/** Tests that do NOT stop containers — shared instance for the whole suite */
class HomeControllerSpec()
    extends PlaySpec
    with DaikokuSpecHelper
    with IntegrationPatience
    with HomeControllerContainers
    with TestContainersForAll {

  override def startContainers(): Containers = createContainers()

  "in TenantMode Default, anyone" can {
    "not check tenant health detailed without access_key" in withContainers {
      case (otoroshi: GenericContainer) and (mailer: GenericContainer) =>
        setupEnvBlocking(
          tenants = Seq(
            tenant.copy(
              mailerSettings = SimpleSMTPSettings(
                host = "localhost",
                port = mailer.mappedPort(1025).toString,
                fromTitle = "testMailerSMTP",
                fromEmail = "noreply@test.io",
                template = None,
                username = None,
                password = None
              ).some,
              otoroshiSettings = Set(
                OtoroshiSettings(
                  id = containerizedOtoroshi,
                  url =
                    s"http://otoroshi.oto.tools:${otoroshi.mappedPort(8080)}",
                  host = "otoroshi-api.oto.tools",
                  clientSecret = otoroshiAdminApiKey.clientSecret,
                  clientId = otoroshiAdminApiKey.clientId
                )
              )
            )
          ),
          users = Seq(daikokuAdmin, tenantAdmin, user),
          teams = Seq(defaultAdminTeam),
          apis = Seq(adminApi)
        )
        val resp = httpJsonCallWithoutSessionBlocking(
          path = s"/health/details"
        )(using tenant)
        resp.status mustBe 401
    }

    "check daikoku health simple without access_key" in withContainers {
      case (otoroshi: GenericContainer) and (mailer: GenericContainer) =>
        setupEnvBlocking(
          tenants = Seq(
            tenant.copy(
              mailerSettings = SimpleSMTPSettings(
                host = "localhost",
                port = mailer.mappedPort(1025).toString,
                fromTitle = "testMailerSMTP",
                fromEmail = "noreply@test.io",
                template = None,
                username = None,
                password = None
              ).some,
              otoroshiSettings = Set(
                OtoroshiSettings(
                  id = containerizedOtoroshi,
                  url =
                    s"http://otoroshi.oto.tools:${otoroshi.mappedPort(8080)}",
                  host = "otoroshi-api.oto.tools",
                  clientSecret = otoroshiAdminApiKey.clientSecret,
                  clientId = otoroshiAdminApiKey.clientId
                )
              )
            )
          ),
          users = Seq(daikokuAdmin, tenantAdmin, user),
          teams = Seq(defaultAdminTeam),
          apis = Seq(adminApi)
        )
        val resp = httpJsonCallWithoutSessionBlocking(
          path = s"/health"
        )(using tenant)
        resp.status mustBe 200
        resp.json mustBe Json.obj("status" -> "ready")
    }

    "check daikoku health like otoroshi" in withContainers {
      case (otoroshi: GenericContainer) and (mailer: GenericContainer) =>
        setupEnvBlocking(
          tenants = Seq(
            tenant.copy(
              mailerSettings = SimpleSMTPSettings(
                host = "localhost",
                port = mailer.mappedPort(1025).toString,
                fromTitle = "testMailerSMTP",
                fromEmail = "noreply@test.io",
                template = None,
                username = None,
                password = None
              ).some,
              otoroshiSettings = Set(
                OtoroshiSettings(
                  id = containerizedOtoroshi,
                  url =
                    s"http://otoroshi.oto.tools:${otoroshi.mappedPort(8080)}",
                  host = "otoroshi-api.oto.tools",
                  clientSecret = otoroshiAdminApiKey.clientSecret,
                  clientId = otoroshiAdminApiKey.clientId
                )
              )
            )
          ),
          users = Seq(daikokuAdmin, tenantAdmin, user),
          teams = Seq(defaultAdminTeam),
          apis = Seq(adminApi)
        )
        val resp = httpJsonCallWithoutSessionBlocking(
          path = s"/health",
          headers = Map("Otoroshi-Health-Check-Logic-Test" -> "100")
        )(using tenant)
        resp.status mustBe 200
        val healthCheckResponse =
          resp.header("Otoroshi-Health-Check-Logic-Test-Result")
        healthCheckResponse.isDefined mustBe true
        healthCheckResponse.get mustBe "142"
    }
  }
}

/** Tests that stop containers to verify "Down" status — fresh containers per test */
class HomeControllerFailoverSpec()
    extends PlaySpec
    with DaikokuSpecHelper
    with IntegrationPatience
    with HomeControllerContainers
    with TestContainersForEach {

  override def startContainers(): Containers = createContainers()

  "in TenantMode Default, anyone" can {
    "check tenant health detailed with services going down" in withContainers {
      case (otoroshi: GenericContainer) and (mailer: GenericContainer) =>
        setupEnvBlocking(
          tenants = Seq(
            tenant.copy(
              mailerSettings = SimpleSMTPSettings(
                host = "localhost",
                port = mailer.mappedPort(1025).toString,
                fromTitle = "testMailerSMTP",
                fromEmail = "noreply@test.io",
                template = None,
                username = None,
                password = None
              ).some,
              otoroshiSettings = Set(
                OtoroshiSettings(
                  id = containerizedOtoroshi,
                  url =
                    s"http://otoroshi.oto.tools:${otoroshi.mappedPort(8080)}",
                  host = "otoroshi-api.oto.tools",
                  clientSecret = otoroshiAdminApiKey.clientSecret,
                  clientId = otoroshiAdminApiKey.clientId
                )
              )
            )
          ),
          users = Seq(daikokuAdmin, tenantAdmin, user),
          teams = Seq(defaultAdminTeam),
          apis = Seq(adminApi)
        )
        val otoroshiPort = otoroshi.mappedPort(8080).toString

        val respAvecMailer = httpJsonCallWithoutSessionBlocking(
          path = s"/health/details?access_key=secret"
        )(using tenant)
        respAvecMailer.status mustBe 200
        respAvecMailer.json mustBe healthResult(otoroshiPort = otoroshiPort)

        mailer.stop()
        val respSansMailer = httpJsonCallWithoutSessionBlocking(
          path = s"/health/details?access_key=secret"
        )(using tenant)
        respSansMailer.json mustBe healthResult(
          otoroshiPort = otoroshiPort,
          mailerStatus = ServiceStatus.Down
        )

        otoroshi.stop()
        val respSansOto = httpJsonCallWithoutSessionBlocking(
          path = s"/health/details?access_key=secret"
        )(using tenant)
        respSansOto.json mustBe healthResult(
          otoroshiPort = otoroshiPort,
          mailerStatus = ServiceStatus.Down,
          otoroshiStatus = ServiceStatus.Down
        )
    }

    "check tenant health detailed in maintenance mode with services going down" in withContainers {
      case (otoroshi: GenericContainer) and (mailer: GenericContainer) =>
        setupEnvBlocking(
          tenants = Seq(
            tenant.copy(
              mailerSettings = SimpleSMTPSettings(
                host = "localhost",
                port = mailer.mappedPort(1025).toString,
                fromTitle = "testMailerSMTP",
                fromEmail = "noreply@test.io",
                template = None,
                username = None,
                password = None
              ).some,
              otoroshiSettings = Set(
                OtoroshiSettings(
                  id = containerizedOtoroshi,
                  url =
                    s"http://otoroshi.oto.tools:${otoroshi.mappedPort(8080)}",
                  host = "otoroshi-api.oto.tools",
                  clientSecret = otoroshiAdminApiKey.clientSecret,
                  clientId = otoroshiAdminApiKey.clientId
                )
              ),
              tenantMode = TenantMode.Maintenance.some
            )
          ),
          users = Seq(daikokuAdmin, tenantAdmin, user),
          teams = Seq(defaultAdminTeam),
          apis = Seq(adminApi)
        )
        val otoroshiPort = otoroshi.mappedPort(8080).toString

        val respAvecMailer = httpJsonCallWithoutSessionBlocking(
          path = s"/health/details?access_key=secret"
        )(using tenant)
        respAvecMailer.status mustBe 200
        respAvecMailer.json mustBe healthResult(
          tenantMode = TenantMode.Maintenance,
          otoroshiPort = otoroshiPort
        )

        mailer.stop()
        val respSansMailer = httpJsonCallWithoutSessionBlocking(
          path = s"/health/details?access_key=secret"
        )(using tenant)
        respSansMailer.json mustBe healthResult(
          tenantMode = TenantMode.Maintenance,
          otoroshiPort = otoroshiPort,
          mailerStatus = ServiceStatus.Down
        )

        otoroshi.stop()
        val respSansOto = httpJsonCallWithoutSessionBlocking(
          path = s"/health/details?access_key=secret"
        )(using tenant)
        respSansOto.json mustBe healthResult(
          tenantMode = TenantMode.Maintenance,
          otoroshiPort = otoroshiPort,
          mailerStatus = ServiceStatus.Down,
          otoroshiStatus = ServiceStatus.Down
        )
    }
  }
}
