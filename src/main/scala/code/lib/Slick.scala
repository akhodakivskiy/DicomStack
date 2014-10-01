package code.lib

import java.util.Properties
import java.sql.Timestamp
import org.joda.time.{DateTime, DateTimeZone}

import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.http.LiftRules
import net.liftweb.util._

import scala.slick.driver.PostgresDriver.simple._
import com.zaxxer.hikari.{HikariDataSource, HikariConfig}
import org.flywaydb.core.Flyway

object Slick extends Loggable {

  trait GenericId {
    def value: Long
    override def toString = value.toString
  }

  case class TSVector(val value: String) extends MappedTo[String]
  case class TSQuery(val value: String) extends MappedTo[String]

  implicit class ColumnTSVectorWrapper(left: Column[TSVector]) {
    def @|(right: Column[TSVector]) = {
      SimpleBinaryOperator[TSVector](" || ").apply(left, right)
    }

    def @@(right: Column[TSQuery]) = {
      SimpleBinaryOperator[Boolean](" @@ ").apply(left, right)
    }
  }

  def tsQuery(c: ConstColumn[String]) = {
    SimpleFunction[TSQuery]("to_tsquery").apply(Seq(c))
  }

  implicit def dateTimeColumnType = {
    MappedColumnType.base[DateTime, Timestamp](
      dt => new Timestamp(dt.getMillis),
      ts => new DateTime(ts.getTime))
  }

  case class StackTraceItem(
    klass: String
  , file: String
  , line: Int
  , method: String
  )

  object StackTraceItem {
    def apply(e: StackTraceElement): StackTraceItem = {
      StackTraceItem(e.getClassName, e.getFileName, e.getLineNumber, e.getMethodName)
    }
  }

  case class ThrowableT(
    message: String
  , cause: Option[ThrowableT]
  , stackTrace: List[StackTraceItem]
  )

  object ThrowableT {
    def apply(t: Throwable): ThrowableT = {
      val stackTrace = t.getStackTrace.toList.map(StackTraceItem.apply)
      val cause = Option(t.getCause).map(ThrowableT.apply)
      ThrowableT(t.getMessage, cause, stackTrace)
    }
    implicit val formats = DefaultFormats
    implicit def importDataColumnType = MappedColumnType.base[ThrowableT, String](
      t => Serialization.write[ThrowableT](t),
      str => Serialization.read[ThrowableT](str)
    )
  }
  import ThrowableT._

  lazy val ddl = {
    code.model.Users.ddl ++
    code.model.UserTokens.ddl ++
    code.model.ExtSessions.ddl ++
    code.model.Files.ddl ++
    code.model.Patients.ddl ++
    code.model.Studies.ddl ++
    code.model.Seriess.ddl ++
    code.model.Images.ddl ++
    code.model.Thumbnails.ddl ++
    code.model.ImportResults.ddl
  }

  def boot = {
    LiftRules.allAround.append(new LoanWrapper {
      override def apply[T](f: => T): T = {
        val resultOrExcept = db.withDynSession {
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
      config.addDataSourceProperty("serverName", serverName);
      config.addDataSourceProperty("databaseName", databaseName);
      config.addDataSourceProperty("user", user);
      config.addDataSourceProperty("password", password);

      config
    }

    val config = configBox.openOrThrowException("Some HikariCP configuration is missing")
    val dataSource = new HikariDataSource(config)

    val flyway = new Flyway()
    flyway.setDataSource(dataSource)
    flyway.setInitOnMigrate(true)
    flyway.migrate

    Database.forDataSource(dataSource)
  }

}
