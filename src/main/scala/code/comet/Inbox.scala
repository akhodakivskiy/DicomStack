package code.comet

import code.model._
import code.lib._

import net.liftweb._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._

import scala.xml._

import ImportKind._

case class InboxItem(title: String)

object InboxServer extends LiftActor with ListenerManager with Loggable {
  def createUpdate = Unit

  override def lowPriority = {
    case (file: FileT, result: ImportResultT) => {
      sendListenersMessage((file, result))
    }
    case msg => logger.warn(s"unexpected message: $msg")
  }
}

class Inbox extends LiftActor with CometListener with Loggable {
  def registerWith = InboxServer

  object inboxItems extends SessionVar[List[InboxItem]](Nil)

  def render = {
    val len = inboxItems.get.length
    "#inboxCount *" #> (if (len > 0) len.toString else "")
  }

  override def lowPriority = {
    case (file: FileT, result: ImportResultT) => {
      inboxItems.update { list =>
        InboxItem(s"Imported file: ${result.fileId.value}") :: list
      }
      partialUpdate(SetHtml("inboxCount", Text(inboxItems.get.length.toString)))
    }
    case msg => logger.warn(s"unexpected message: $msg")
  }
}
