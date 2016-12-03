package code.lib

import java.sql.Timestamp

import code.model._
import com.zaxxer.hikari.{HikariDataSource, HikariConfig}
import net.liftweb.http.LiftRules
import net.liftweb.util.{Props, LiftFlowOfControlException, LoanWrapper}
import org.flywaydb.core.Flyway
import org.joda.time.DateTime

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.AbstractTable

object Slick {

  trait GenericId {
    def value: Long
    override def toString = value.toString
  }

  implicit def dateTimeColumnType: BaseColumnType[DateTime] = {
    MappedColumnType.base[DateTime, Timestamp](
      dt => new Timestamp(dt.getMillis),
      ts => new DateTime(ts.getTime))
  }

  def boot() = {
    LiftRules.allAround.append(new LoanWrapper {
      override def apply[T](f: => T): T = {
        val resultOrExcept = db.withDynTransaction {
          try {
            Right(f)
          } catch {
            case e: LiftFlowOfControlException => Left(e)
          }
        }

        resultOrExcept match {
          case Right(result) => result
          case Left(except) => throw except
        }
      }
    })
  }

  val ddl = {
    Ideas.ddl ++ IdeaTexts.ddl ++ IdeaImages.ddl ++
    IdeaSignups.ddl ++ Users.ddl ++ UserTokens.ddl ++ UserAuthInfos.ddl
  }

  lazy val db = {
    val configBox = for {
      clsName <- Props.get("dataSourceClassName")
      serverName <- Props.get("dataSource.serverName")
      databaseName <- Props.get("dataSource.databaseName")
      user <- Props.get("dataSource.user")
      password <- Props.get("dataSource.password")
    } yield {
      val config = new HikariConfig()
      config.setDataSourceClassName(clsName)
      config.addDataSourceProperty("serverName", serverName)
      config.addDataSourceProperty("databaseName", databaseName)
      config.addDataSourceProperty("user", user)
      config.addDataSourceProperty("password", password)

      config
    }

    val config = configBox.openOrThrowException("Some HikariCP configuration is missing")
    val dataSource = new HikariDataSource(config)

    val flyway = new Flyway()
    flyway.setDataSource(dataSource)
    flyway.setBaselineOnMigrate(true)
    flyway.migrate

    Database.forDataSource(dataSource)
  }
}
