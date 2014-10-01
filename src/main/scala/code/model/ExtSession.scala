package code.model

import org.joda.time.{DateTime, DateTimeZone}
import java.util.TimeZone

import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.provider._
import net.liftweb.common._

import code.lib._

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import Slick._

case class ExtSessionId(val value: Long) extends MappedTo[Long] with GenericId

object ExtSessionId {
  val empty = new ExtSessionId(-1)
}

case class ExtSession(
  cookieId: String
, userId: UserId
, expiresAt: DateTime
, active: Boolean = true
, id: ExtSessionId = ExtSessionId.empty
)

class ExtSessionsTable(tag: Tag) extends Table[ExtSession](tag, "ext_sessions") {
  def cookie = column[String]("cookie")
  def userId = column[UserId]("user_id")
  def expiresAt = column[DateTime]("expires_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  def active = column[Boolean]("is_active")

  def id = column[ExtSessionId]("id", O.PrimaryKey, O.AutoInc)

  def userId_idx = index("ext_sessions_user_id__idx", userId)
  def cookie_idx = index("ext_sessions_cookie__idx", cookie, unique = true)

  def user = foreignKey("ext_sessions_user_id__fk", userId, Users)(_.id,
      onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (cookie, userId, expiresAt, active, id) <>
          (ExtSession.tupled, ExtSession.unapply)
}

object ExtSessions extends TableQuery(new ExtSessionsTable(_)) with Loggable {

  val returningId = this returning map(_.id)

  def insert(es: ExtSession) = {
    val id = returningId.insert(es)
    es.copy(id = id)
  }

  def byCookie(cookie: String) = filter(_.cookie === cookie).filter(_.active)

  def expired = filter(_.expiresAt < DateTime.now).filter(_.active)

  private val CookieLength = 32
  private val CookieName = "JSESSIONID_EXT"
  private val CookieGCProbability = 0.05
  private val CookieAge = 5.days

  private object _user extends SessionVar[Box[UserT]](Empty)
  // This is to hack around the issue with S.addCookie in earlyInStateful
  private object _isSessionSetupHack extends SessionVar[Boolean](false)

  def user: Box[UserT] = _user.get
  def isLoggedIn: Boolean = user.isDefined

  def logIn(user: UserT, extended: Boolean) = {
    deleteExtSession
    _user(Full(user))

    if (extended) {
      val cookieId = randomString(CookieLength)
      val cookieAge = (CookieAge / 1000L).toInt
      val cookie = HTTPCookie(CookieName, cookieId)
        .setMaxAge(cookieAge)
        .setPath("/")
      S.addCookie(cookie)

      val date = new DateTime(millis + CookieAge)
      ExtSessions.insert(ExtSession(cookieId, user.id, date))
    }
  }

  def logOut = {
    _user.remove
    S.session.foreach(_.destroySession)
    deleteExtSession
  }

  def deleteExtSession = {
    for (cookie <- S.findCookie(CookieName)) {
      S.deleteCookie(cookie)
      cookie.value.map { cookieId: String =>
        ExtSessions.byCookie(cookieId).map(_.active).update(false)
      }
    }

    if (shouldShow(CookieGCProbability)) {
      ExtSessions.expired.map(_.active).update(false)
      logger.info(s"Garbage collecting inactive sessions")
    }
  }

  def testCookieEarlyInStateful: Box[Req] => Unit = {
    ignoredReq => {
      if (_isSessionSetupHack.get) {
        (user, S.findCookie(CookieName)) match {
          case (Empty, Full(c)) => {
            c.value.map { cookieId =>
              val user = for {
                es <- ExtSessions.byCookie(cookieId)
                user <- es.user
              } yield user

              user.firstOption match {
                case Some(user) => logIn(user, true)
                case None => S.deleteCookie(CookieName)
              }
            }
          }
          case _ => Unit
        }
      }
    }
  }


  object localTimeZone extends SessionVar[Box[DateTimeZone]](Empty)

  def boot = {
    LiftSession.onSetupSession ::= { _: LiftSession =>
      _isSessionSetupHack(true)
    }

    LiftRules.earlyInStateful.append(testCookieEarlyInStateful)

    LiftRules.liftRequest.append {
      case Req("classpath" :: _, _, _) => true
      case Req("ajax_request" :: _, _, _) => true
      case Req("comet_request" :: _, _, _) => true
      case Req("favicon" :: Nil, "ico", GetRequest) => false
      case Req(_, "css", GetRequest) => false
      case Req(_, "js", GetRequest) => false
    }

    LiftRules.timeZoneCalculator = { reqBox =>
      localTimeZone.get.map(_.toTimeZone).openOr {
        LiftRules.defaultTimeZoneCalculator(reqBox)
      }
    }
  }
}
