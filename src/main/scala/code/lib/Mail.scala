package code.lib

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http._
import javax.mail.{Authenticator,PasswordAuthentication}
import scala.xml.NodeSeq

import code.model._

object Mail extends Loggable {
  import Mailer._

  val noReply = s"no-reply@${S.hostName}"

  def boot = {
    var isAuth = Props.get("mail.smtp.auth", "false").toBoolean

    if (isAuth) {
      (Props.get("mail.user"), Props.get("mail.password")) match {
        case (Full(username), Full(password)) =>
          Mailer.authenticator = Full(new Authenticator() {
            override def getPasswordAuthentication = new
            PasswordAuthentication(username, password)
          })
        case _ => new Exception("Username/password not supplied for Mailer.")
      }
    }
  }

  def sendSignupEmail(user: UserT, token: UserTokenT) = {
    val subject = "Activate account"
    val to = user.email
    val url = S.hostAndPath + Site.activate.calcHref(token)
    S.runTemplate("emails-hidden" :: "signup" :: Nil, ("Url", "*" #> url)) match {
      case Full(html) => sendMail(noReply, to, noReply, subject, html)
      case e => logger.error(e)
    }
  }

  def sendForgotPasswordEmail(user: UserT, token: UserTokenT) = {
    val subject = "Reset Password"
    val to = user.email
    val url = S.hostAndPath + Site.resetPassword.calcHref(token)
    S.runTemplate("emails-hidden" :: "reset-password" :: Nil, ("Url", "*" #> url)) match {
      case Full(html) => sendMail(noReply, to, noReply, subject, html)
      case e => logger.error(e)
    }
  }

  def sendMail(from: String, to: String, replyTo: String, subject: String, html: NodeSeq) = {
    Mailer.sendMail(
      From(from),
      Subject(subject),
      (XHTMLMailBodyType(html) :: To(to) :: ReplyTo(replyTo) :: Nil) : _*)
  }
}
