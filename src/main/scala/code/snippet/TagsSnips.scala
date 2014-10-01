package code.snippet

import code.lib._
import code.model._
import net.liftweb.common._
import net.liftweb.util.Helpers._

import scala.concurrent._

class TagsPage(file: FileT) extends BootstrapSortedPaginatorSnippet[TagValue, String]
                            with Loggable {
  val tags = Await.result(Defer.tagsFuture(file), duration.Duration.Inf)

  logger.info(tags.size.toString)

  override def itemsPerPage = count.toInt
  override def count = tags.size
  override def page = {
    (sort._1 match { 
      case 0 => tags.sortBy(_.element).sortBy(_.group)
      case 1 => tags.sortBy(_.name)
      case 2 => tags.sortBy(_.value)
      case 3 => tags.sortBy(_.vr)
      case _ => tags
    }, sort._2) match {
      case (tags, false) => tags.reverse
      case (tags, true) => tags
    }
  }

  override def headers = List("tag", "name", "value", "vr").map(c => (c, c))

  def render = {
    "tr" #> page.map { tv =>
      "role=tagGroup *" #> "%04X".format(tv.group) &
      "role=tagElement *" #> "%04X".format(tv.element) &
      "role=tagName *" #> tv.name &
      "role=tagValue *" #> tv.value &
      "role=tagVR *" #> tv.vr
    }
  }
}
