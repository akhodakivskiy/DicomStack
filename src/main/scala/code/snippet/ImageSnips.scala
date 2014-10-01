package code.snippet

import code.lib._
import code.lib.MyJsCmds._
import code.model._

import scala.concurrent._

import scala.xml._

import java.io.InputStream

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.jquery.JqJsCmds._
import net.liftweb.util._
import net.liftweb.util.Helpers._

class ImageDetails(image: ImageT) extends Loggable {
  lazy val dicomOpt = Images.findDicomEntities(image.id)

  lazy val imageOpt = dicomOpt.map(_._1)
  lazy val seriesOpt = dicomOpt.map(_._2)
  lazy val studyOpt = dicomOpt.map(_._3)
  lazy val patientOpt = dicomOpt.map(_._4)

  def renderPatient = patientOpt.map { patient: PatientT =>
    "role=patientName *" #> patient.fullName &
    "role=patientSex *" #> patient.sex.toString &
    "role=patientIdentifier *" #> patient.identifier &
    "role=patientBirthDate *" #> Util.DTF.localShort.print(patient.birthDate)
  }.getOrElse(PassThru)

  def renderStudy = studyOpt.map { study: StudyT =>
    "role=studyDescription *" #> study.description &
    "role=studyInstanceUid *" #> study.instanceUid &
    "role=studyDate *" #> Util.DTF.localShort.print(study.date)
  }.getOrElse(PassThru)

  def renderSeries = seriesOpt.map { series: SeriesT =>
    "role=seriesDescription *" #> series.description &
    "role=seriesInstanceUid *" #> series.instanceUid &
    "role=seriesDate *" #> Util.DTF.localShort.print(series.date) &
    "role=seriesNumber *" #> series.number
  }.getOrElse(PassThru)

  def renderImage = imageOpt.map { image: ImageT =>
    "role=imageInstanceNumber *" #> image.instanceNumber &
    "role=imageRows *" #> image.rows &
    "role=imageColumns *" #> image.columns &
    "role=imageTransferSyntax *" #> image.transferSyntax
  }.getOrElse(PassThru)
}

class FullImage(image: ImageT) extends Loggable {
  val thumbBox = Thumbnails.largestByImageId(image.id)

  def render = {
    logger.info(thumbBox)
    "img [alt]" #> image.sopInstanceUid &
    "img [src]" #> thumbBox.map(Site.library.thumbnail.calcHref(_))
  }
}
