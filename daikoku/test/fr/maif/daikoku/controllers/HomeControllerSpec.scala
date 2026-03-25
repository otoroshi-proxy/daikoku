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
import play.api.libs.json.Json

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

  val pwd = System.getProperty("user.dir")

  def healthResult(
      tenantMode: TenantMode = TenantMode.Default,
      datastoreStatus: ServiceStatus = ServiceStatus.Up,
      mailerStatus: ServiceStatus = ServiceStatus.Up,
      s3Status: ServiceStatus = ServiceStatus.Absent,
      otoroshiStatus: ServiceStatus = ServiceStatus.Up,
      otoroshiPort: String
  ) = Json.obj(
    "datastore" -> datastoreStatus.value,
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

  "in TenantMode Default, anyone with a key" can {
    "check tenant health " in withContainers {

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
        val session = loginWithBlocking(daikokuAdmin, tenant)
        val respAvecMailer = httpJsonCallBlocking(
          path = s"/health"
        )(tenant, session)
        respAvecMailer.status mustBe 200
        respAvecMailer.json mustBe healthResult(otoroshiPort = otoroshiPort)
        mailer.stop()

        val respSansMailer = httpJsonCallBlocking(
          path = s"/health"
        )(tenant, session)
        respSansMailer.json mustBe healthResult(
          otoroshiPort = otoroshiPort,
          mailerStatus = ServiceStatus.Down
        )

        otoroshi.stop()
        val respSansOto = httpJsonCallBlocking(
          path = s"/health"
        )(tenant, session)
        respSansOto.json mustBe healthResult(
          otoroshiPort = otoroshiPort,
          mailerStatus = ServiceStatus.Down,
          otoroshiStatus = ServiceStatus.Down
        )
    }

    "check tenant health even in maintenance mode" in withContainers {

      case (otoroshi: GenericContainer) and (mailer: GenericContainer) =>
        otoroshi.start()
        mailer.start()
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
        val session = loginWithBlocking(daikokuAdmin, tenant)
        val respAvecMailer = httpJsonCallBlocking(
          path = s"/health"
        )(tenant, session)
        respAvecMailer.status mustBe 200
        respAvecMailer.json mustBe healthResult(
          tenantMode = TenantMode.Maintenance,
          otoroshiPort = otoroshiPort
        )
        mailer.stop()

        val respSansMailer = httpJsonCallBlocking(
          path = s"/health"
        )(tenant, session)
        respSansMailer.json mustBe healthResult(
          tenantMode = TenantMode.Maintenance,
          otoroshiPort = otoroshiPort,
          mailerStatus = ServiceStatus.Down
        )

        otoroshi.stop()
        val respSansOto = httpJsonCallBlocking(
          path = s"/health"
        )(tenant, session)
        respSansOto.json mustBe healthResult(
          tenantMode = TenantMode.Maintenance,
          otoroshiPort = otoroshiPort,
          mailerStatus = ServiceStatus.Down,
          otoroshiStatus = ServiceStatus.Down
        )
    }
  }
}
