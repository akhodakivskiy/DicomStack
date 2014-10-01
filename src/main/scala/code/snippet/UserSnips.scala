package code.snippet

import code.model._
import code.lib._
import code.lib.validation._

import org.joda.time.{DateTime, DateTimeZone}

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.builtin.snippet._

object UserSnips {
  def requireLogin(seq: NodeSeq) =
    if (ExtSessions.isLoggedIn) seq else NodeSeq.Empty

  def requireAnon(seq: NodeSeq) =
    if (!ExtSessions.isLoggedIn) seq else NodeSeq.Empty

  def name = {
    ExtSessions.user match {
      case Full(u) => "* *" #> u.email
      case _ => "* *" #> "Guest"
    }
  }
}

class UserSignup extends StatefulSnippet with Loggable {
  def dispatch = { case "render" => render }

  private var email = ""

  def render = {
    def process() = Slick.db.withDynSession {
      Users.findByEmail(email) orElse {
        val newUser = UserT(email)
        Users.email.validate2(newUser)(ValError.flash) {
          logger.info(newUser.createdAt)
          Users.insert(newUser)
        }
      } match {
        case Some(user) => {
          val token = UserTokens.find(user.id, TokenType.Activate) getOrElse {
            UserTokens.insert(UserTokenT(user.id, TokenType.Activate))
          }
          Mail.sendSignupEmail(user, token)
        }
        case _ => logger.error("Signup process failed")
      }
      S.notice("Please check your email for activation instructions")
      S.seeOther(Site.index.loc.calcDefaultHref)
    }

    "#inputEmail" #> SHtml.email(email, (e: String) => email = e.toLowerCase) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
}

class UserActivate(token: UserTokenT) extends StatefulSnippet with Loggable {
  def dispatch = {
    case "render" => render
    case "email" => email
  }

  val userOpt = Users.findInactive(token.userId)

  private var first = ""
  private var last = ""
  private var password = ""
  private var confirmPassword = ""

  def email = "* *" #> userOpt.map(_.email)

  def render = {
    def process() = {
      if (password != confirmPassword) {
        S.error("confirmPassword", "Confirmation doesn't match the password")
      } else {
        userOpt map { user =>
          user.copy(
            password = password
          , first = first
          , last = last
          , active = true)
        } flatMap { user =>
          Users.validate2(user)(ValError.flash) {
            user.copy(password = Users.hashpw(password))
          }
        } map { user =>
          UserTokens.update(token.copy(active = false))
          Users.update(user)

          ExtSessions.logIn(user, false)
          S.notice("Your account is now active. You are now logged in!")
          S.seeOther(Site.index.loc.calcDefaultHref)
        }
      }
    }

    "#inputFirst" #> SHtml.text(first, first = _) &
    "#inputLast" #> SHtml.text(last, last = _) &
    "#inputPassword" #> SHtml.password("", password = _) &
    "#inputConfirmPassword" #> SHtml.password("", confirmPassword = _) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
}

class UserLogin extends StatefulSnippet with Loggable {
  def dispatch = { case "render" => render }

  private var email = ""
  private var password = ""
  private var timeZone = ""
  private var remember = false

  def render = {
    def process() {
      Users.findActiveByEmail(email) map { user =>
        logger.info(s"$email, $password - ${user.password}" + Users.checkpw(password, user.password))
        user
      } match {
        case Some(user) if (Users.checkpw(password, user.password)) => {
          ExtSessions.logIn(user, remember)
          S.seeOther(Site.index.loc.calcDefaultHref)
        }
        case _ => S.error("Wrong email or password")
      }
    }

    "#inputEmail" #> SHtml.text(email, email = _) &
    "#inputPassword" #> SHtml.password("", password = _) &
    "#inputRemember" #> SHtml.checkbox(remember, remember = _) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
}

class UserForgotPassword extends StatefulSnippet with Loggable {
  def dispatch = { case "render" => render }

  private var email = ""

  def render = {
    def process() {
      Users.findByEmail(email) match {
        case Some(user) => {
          UserTokens.deactivateByUserId(user.id, TokenType.ResetPassword)

          val token = UserTokens.insert(UserTokenT(user.id, TokenType.ResetPassword))

          Mail.sendForgotPasswordEmail(user, token)
        }
        case _ => logger.trace("User not found")
      }
      S.notice("Please check your email for password recovery instructions")
      S.seeOther(Site.login.loc.calcDefaultHref)
    }

    "#inputEmail" #> SHtml.text(email, email = _) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
}

class UserResetPassword(token: UserTokenT) extends StatefulSnippet with Loggable {
  def dispatch = { case "render" => render }

  private var password = ""
  private var confirmPassword = ""

  def render = {
    def process() {
      if (password != confirmPassword) {
        S.error("confirmPassword", "Confirmation doesn't match the password")
      } else {
        Users.find(token.userId) map { user =>
          user.copy(password = password)
        } flatMap { user =>
          Users.password.validate2(user)(ValError.flash) {
            user.copy(password = Users.hashpw(password))
          }
        } match {
          case Some(user) => {
            Users.update(user)
            S.notice("Your password has been successfully changed. You can login now")
          }
          case None => {
            S.error("Failed to change password")
          }
        }
        S.seeOther(Site.index.loc.calcDefaultHref)
      }
    }

    "#inputPassword" #> SHtml.password("", password = _) &
    "#inputConfirmPassword" #> SHtml.password("", confirmPassword = _) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
}

object Jstz extends Loggable {
  def render = {
    val defaultTz = DateTimeZone.forTimeZone(S.timeZone)
    ExtSessions.localTimeZone.get match {
      case Empty => {
        ExtSessions.localTimeZone(Full(defaultTz))
        S.appendJs {
          val determineCmd = JE.Call("jstz.determine") ~> JE.JsFunc("name")
          SHtml.ajaxCall(determineCmd, { tzId =>
            val tz = tryo(logger.info(_:Throwable)) {
              DateTimeZone.forID(tzId)
            } openOr(defaultTz)
            ExtSessions.localTimeZone(Full(tz))
            JsCmds.Noop
          }).cmd
        }
        PassThru
      }
      case _ => ClearNodes
    }
  }
}
