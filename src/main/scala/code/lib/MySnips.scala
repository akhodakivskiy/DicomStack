package code.lib

import scala.xml._

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.http.js._

case class Activator(tag: String, cls: String) {
  var currentId: Option[String] = None

  def apply(activate: Boolean)(cmd: () => JsCmd) = {
    val id = nextFuncName
    val result = {
      s"$tag [id]" #> id &
      s"$tag [onclick]" #> SHtml.ajaxInvoke { () =>
        val oldId = currentId
        currentId = Some(id)
        MyJsCmds.JqActivate(currentId, oldId, cls) & cmd()
      }
    }

    activate match {
      case true => {
        currentId = Some(id)
        result & s"$tag [class+]" #> cls
      }
      case _ => result
    }
  }
}

trait EmptyableSnippet extends Loggable {
  protected def isEmpty: Boolean

  def ifEmpty = if (isEmpty) PassThru else ClearNodes
  def ifNotEmpty = if (!isEmpty) PassThru else ClearNodes
}

trait BootstrapSortedPaginatorSnippet[T, C] extends SortedPaginatorSnippet[T, C] with EmptyableSnippet {
  override def isEmpty = (count == 0)

  override def firstXml: NodeSeq = Unparsed("&laquo;")
  override def lastXml: NodeSeq = Unparsed("&raquo;")
  override def pageXml(newFirst: Long, ns: NodeSeq): NodeSeq = {
    if (first == newFirst) {
      <li class="disabled"><a href="#">{ns}</a></li>
    } else if (newFirst < 0 || newFirst >= count) {
      <li class="active"><a href="#">{ns}</a></li>
    } else {
      <li><a href={pageUrl(newFirst)}>{ns}</a></li>
    }
  }
  override def zoomedPages = (
    List(curPage - 120, curPage - 20) ++
    (curPage - 7 to curPage + 7) ++
    List(curPage + 20, curPage + 120)
  ) filter { n => n >= 0 && n < numPages }

  val hintAlpha = "hintalpha"
  val hintNum = "hintnum"

  def hints = List(hintAlpha, hintNum)
  def hintHtml(hint: String, asc: Boolean) = {
    val cls = (hint, asc) match {
      case (h, true) if h == hintAlpha => "fa fa-sort-alpha-asc"
      case (h, false) if h == hintAlpha => "fa fa-sort-alpha-desc"
      case (h, true) if h == hintNum => "fa fa-sort-numeric-asc"
      case (h, false) if h == hintNum => "fa fa-sort-numeric-desc"
    }
    <span class={cls}></span>
  }

  override def paginate(xhtml: NodeSeq): NodeSeq = {
    hints.foldLeft(super.paginate(xhtml)) { case (xhtmlInner, hint) =>
      val funcs = headers.zipWithIndex.map { case ((colName, _), colIdx) =>
        TheBindParam(colName, sort match {
          case (idx, asc) if idx == colIdx => hintHtml(hint, asc)
          case _ => NodeSeq.Empty
        })
      }
      bind(hint, xhtmlInner, funcs.toSeq : _*)
    }
  }
}
