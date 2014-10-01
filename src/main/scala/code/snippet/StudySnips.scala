package code.snippet

import code.model._
import code.lib._

import java.io.InputStream

import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._

object StudySnips {
  import scala.slick.driver.PostgresDriver.simple._
  import Database.dynamicSession
  import Slick._

  val seriesImageCounts = Compiled { (studyId: Column[StudyId]) =>
    ((for {
      s <- Seriess if s.studyId === studyId && !s.isRemoved
      i <- Images if i.seriesId === s.id && !i.isRemoved
    } yield (s, i)).groupBy(_._2.seriesId).map { case (seriesId, sis) =>
      (seriesId, sis.map(_._2).length)
    })
  }

  def findSeriesWithImageCounts(studyId: StudyId): List[(SeriesT, Int)] = {
    val sic = seriesImageCounts(studyId).list
    Seriess.findByStudyId(studyId).map { series =>
      val imageCount = sic.find(_._1 == series.id).map(_._2).getOrElse(0)
      (series, imageCount)
    }
  }

  def findImagesWithThumbnail(seriesId: SeriesId, dim: Int) = {
    val images = Images.findBySeriesId(seriesId)
    val thumbnails = Thumbnails.findBySeriesId(seriesId)
    images.map { i =>
      val t = thumbnails.filter(_.imageId == i.id)
        .sortBy(t => (t.dimension - dim).abs).headOption
      (i, t)
    }
  }
}

class StudySnips(study: StudyT) extends StatefulSnippet with Loggable {
  def dispatch = {
    case "name" => name
    case "details" => details
    case "sizes" => sizes
    case "series" => series
    case "images" => images
  }

  // Study should have a patient
  val patient = Patients.find(study.patientId).head
  val seriesWithImageCounts = StudySnips.findSeriesWithImageCounts(study.id)

  var imagesDiv: Option[IdMemoizeTransform] = None

  object currentSeries extends RequestVar[Box[SeriesT]](Seriess.firstByStudyId(study.id))
  val seriesActivator = Activator("a", "active")

  val allSizes = List(256 -> "small", 512 -> "large")
  object currentSize extends SessionVar[Int](allSizes.head._1)
  val sizeActivator = Activator("button", "active")

  def name = "* *" #> study.description

  def details = {
    "role=patientName *" #> patient.fullName &
    "role=modality *" #> study.modality &
    "role=date *" #> Util.DTF.localShort.print(study.date)
  }

  def sizes = {
    allSizes map { case (size, sizeName) =>
      s"role=$sizeName" #> sizeActivator(currentSize.get == size) { () =>
        currentSize.set(size)
        imagesDiv.map(_.setHtml).getOrElse(JsCmds.Noop)
      }
    } reduce((one, two) => one & two)
  }

  def series = "role=series" #> {
    seriesWithImageCounts.map { case (series, imagesCount) =>
      val current = currentSeries.get.map(_.id == series.id).openOr(false)

      seriesActivator(current) { () =>
        currentSeries.set(Some(series))
        imagesDiv.map(_.setHtml).getOrElse(JsCmds.Noop)
      } &
      "role=series [href]" #> s"#${series.id}" &
      "role=seriesDescription *" #> Util.maybeUnnamed(series.description, Util.unnamedSeries) &
      "role=seriesImagesCount *" #> imagesCount
    }
  }

  def images = SHtml.idMemoize { div =>
    def sizeClass = currentSize.get match {
      case 256 => "col-md-4 col-sm-6 col-xs-6"
      case _ => "col-md-6 col-sm-12 col-xs-12"
    }

    imagesDiv = Some(div)
    S.setHeader("Access-Control-Allow-Origin", "*")

    currentSeries map { series =>
      "div [class]" #> sizeClass &
      "*" #> {
        val iwt = StudySnips.findImagesWithThumbnail(series.id, currentSize.get)
        "role=image" #> iwt.map { case (image, thumbnail) =>
          "a [href]" #> Site.library.image.calcHref(image) &
          thumbnail.map { t =>
            "img [src]" #> Site.library.thumbnail.calcHref(t) &
            "img [alt]" #> image.sopInstanceUid
          }.getOrElse {
            "img [alt]" #> "Image cannot be displayed"
          }
        }
      }
    } getOrElse "*" #> "No Images in this series"
  }
}
