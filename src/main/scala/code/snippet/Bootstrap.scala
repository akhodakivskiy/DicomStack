package code.snippet

import code.lib._

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.sitemap._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.builtin.snippet._

object BootstrapMsgs extends Loggable {
  // Helper type
  type Msg = (NodeSeq, Box[String])

  // Only render messages that don't have an id
  def render = {

    // Strip messages that have an id
    def noId(msg: Msg): Box[NodeSeq] = msg match {
      case (seq, str) => str match {
        case Empty => Full(seq)
        case _ => Empty
      }
    }

    (S.attr("type").openOr("") match {
      case "notice" => S.notices
      case "warning" => S.warnings
      case "error" => S.errors
      case _ => List[Msg]()
    }).flatMap(noId) match {
      case List() => ClearNodes
      case msgs => "* *" #> msgs
    }
  }

  def has = {
    S.attr("name").map(name => S.messagesById(name) _) match {
      case Full(msgs) if (msgs(S.errors).length > 0) => ".form-group [class+]" #> "has-error"
      case Full(msgs) if (msgs(S.warnings).length > 0) => ".form-group [class+]" #> "has-warning"
      case Full(msgs) if (msgs(S.notices).length > 0) => ".form-group [class+]" #> "has-success"
      case _ => PassThru
    }
  }

  def show = {
    S.attr("name").map(n => S.messagesById(n) _) match {
      case Full(msgs) => "span *" #> msgs(S.getNotices.map(a => (a._2, a._3)))
      case _ => "span" #> ""
    }
  }
}

object BootstrapMenu extends DispatchSnippet with Loggable {
  def dispatch = {
    case "group" => group
    case "item" => item
  }

  def isCurrent(name:String): Boolean = {
    (for {
      request <- S.request
      loc <- request.location
    } yield {
      loc.breadCrumbs
    }).map(locs => {
      locs.foldLeft(false)((m, loc) => m || loc.name == name)
    }).getOrElse(false)
  }

  def bindLoc(loc: Loc[_]) : CssSel = {
    "a [href]" #> loc.calcDefaultHref &
    "role=locTitle *" #> loc.linkText &
    "li [class+]" #> (if (isCurrent(loc.name)) "active" else "")
  }

  def group = {
    (for {
      name <- S.attr("name")
      siteMap <- LiftRules.siteMap
    } yield {
      "ul *" #> siteMap.locForGroup(name).map(bindLoc)
    }).openOr(ClearNodes)
  }

  def item = {
    (for {
      name <- S.attr("name")
      loc <- SiteMap.findAndTestLoc(name)
    } yield {
      bindLoc(loc)
    }).openOr(ClearNodes)
  }
}

class BootstrapBreadcrumbs extends Loggable {
  def render = {
    S.location.map { currentLoc =>
      val items = currentLoc.breadCrumbs map { loc =>
        loc.name match {
          case name if (name == currentLoc.name) => <li class="active">{loc.title}</li>
          case _ => <li><a href={loc.calcDefaultHref}>{loc.title}</a></li>
        }
      }
      "*" #> <ol class="breadcrumb">{items}</ol>
    } openOr ClearNodes
  }
}
