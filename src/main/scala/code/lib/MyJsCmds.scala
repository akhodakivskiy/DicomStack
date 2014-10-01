package code.lib

import net.liftweb.common._
import net.liftweb.http.js._
import net.liftweb.http.js.jquery._
import net.liftweb.http.js.jquery.JqJE._

import scala.xml.NodeSeq

object MyJsCmds {

  case class InsertAfter(id: String, content: NodeSeq) extends JsCmd with JsMember {
    override def toJsCmd = (JqId(id) ~> JqAppend(content)).toJsCmd
  }

  case class JqSetAttr(attr: String, value: Box[_]) extends JsCmd with JsMember {
    override def toJsCmd = value match {
      case Full(a) => s"attr('$attr', '${a.toString}');"
      case _ => s"removeAttr('$attr');"
    }
  }

  case class SetAttr(id: String, attr: String, value: Box[_]) extends JsCmd {
    override def toJsCmd = (JqId(id) ~> JqSetAttr(attr, value)).toJsCmd
  }

  case class JqAddClass(cls: String) extends JsCmd with JsMember {
    override def toJsCmd = s"addClass('$cls');"
  }

  case class AddClass(id: String, cls: String) extends JsCmd {
    override def toJsCmd = (JqId(id) ~> JqAddClass(cls)).toJsCmd
  }

  case class JqRemoveClass(cls: String) extends JsCmd with JsMember {
    override def toJsCmd = s"removeClass('$cls');"
  }

  case class RemoveClass(id: String, cls: String) extends JsCmd {
    override def toJsCmd = (JqId(id) ~> JqRemoveClass(cls)).toJsCmd
  }

  case class JqActivate(
    newId: Option[String]
  , oldId: Option[String]
  , cls: String = "active"
  ) extends JsCmd {
    def toJsCmd = (
      List(
        oldId.map(id => (JqId(id) ~> JqRemoveClass(cls)).cmd)
      , newId.map(id => (JqId(id) ~> JqAddClass(cls)).cmd)
      ).flatten.foldLeft(JsCmds.Noop)((a, b) => a & b)
    ).toJsCmd
  }

  abstract class JqFlashMessage(id: String, msg: NodeSeq, kind: String) extends JsCmd {
    private val klass = s"alert alert-$kind"
    def toJsCmd = (JqId(id) ~> JqAppend(<div class={klass}>{msg}</div>)).toJsCmd
  }

  case class JqSuccess(id: String, msg: NodeSeq) extends JqFlashMessage(id, msg, "success")
  case class JqNotice(id: String, msg: NodeSeq) extends JqFlashMessage(id, msg, "info")
  case class JqWarning(id: String, msg: NodeSeq) extends JqFlashMessage(id, msg, "warning")
  case class JqError(id: String, msg: NodeSeq) extends JqFlashMessage(id, msg, "danger")
}
