package code.snippet

import code.model._
import code.lib._

import java.io.InputStream

import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.sitemap._

object IgnoreSearch {
  def render = ClearNodes
}

object SearchSnips extends Loggable {

  // Rebuilds the uri replacing the `q` parameter
  def mkSearchUri(path: String, params: Map[String, List[String]], q: Box[String]) = {
    val uri = params.foldLeft(path) {
      case (uri, ("q", _)) => uri
      case (uri, (name, values)) => {
        values.foldLeft(uri) { (paramUri, value) =>
          appendParams(paramUri, List(name -> value))
        }
      }
    }
    q.filter(!_.isEmpty).map { q =>
      appendParams(uri, List("q" -> q))
    }.openOr(uri)
  }

  def render = {
    var term = S.param("q").openOr("")
    val params = S.request.map(_.params).openOr(sys.error("Params are missing"))
    val path = S.location.map(_.calcDefaultHref).openOr(sys.error("Location is missing"))

    "role=searchTerm" #> SHtml.text(term, term = _) &
    "role=reset" #> (if (term.isEmpty) ClearNodes else PassThru) &
    "role=reset [href]" #> mkSearchUri(path, params, Empty) &
    "role=search" #> SHtml.onSubmitUnit { () =>
      logger.info(term)
      S.redirectTo(mkSearchUri(path, params, Full(term)))
    }
  }
}
