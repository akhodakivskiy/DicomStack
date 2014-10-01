package code.lib

import code.model._

import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.util.{Calendar, GregorianCalendar, TimeZone}
import javax.imageio.ImageIO
import javax.imageio.stream.ImageOutputStream
import net.liftweb.common._
import net.liftweb.util.Helpers._
import org.dcm4che3.data.{StandardElementDictionary, Keyword, Attributes, VR, Tag, Fragments, Sequence}
import org.dcm4che3.imageio.codec.ImageReaderFactory
import org.dcm4che3.imageio.codec.ImageReaderFactory
import org.dcm4che3.imageio.plugins.dcm.{DicomImageReaderSpi, DicomImageReader}
import org.dcm4che3.io.{DicomInputStream, DicomInputHandler, DicomStreamException}
import org.dcm4che3.util.{DateUtils, TagUtils}
import org.joda.time.{DateTime, DateTimeZone}
import scala.collection.JavaConversions._

case class TagValue(group: Int, element: Int, name: String, vr: String, value: String)

case class DicomImportData(
  isNewPatient: Boolean
, isNewStudy: Boolean
, isNewSeries: Boolean
, isNewImage: Boolean
)

case class DicomEntities(
  patient: PatientT
, study: StudyT
, series: SeriesT
, image: ImageT
)

object Dicom extends Logger {
  def readEntities(file: FileT, is: InputStream): Box[DicomEntities] = {
    trace("Reading DICOM file")
    tryo {
      val dis = new DicomInputStream(is)
      val attrs = dis.readDataset(-1, -1)
      (attrs, dis.getTransferSyntax)
    } filter { case (attrs, transferSyntax) =>
      !attrs.contains(Tag.DirectoryRecordSequence)
    } map { case (attrs, transferSyntax) =>
      val patient = Patients.fromAttrs(attrs)
      val study = Studies.fromAttrs(attrs)
      val series = Seriess.fromAttrs(attrs)
      val image = Images.fromAttrs(attrs, transferSyntax)
      DicomEntities(patient, study, series, image)
    }
  }

  def readOrSaveEntities(file: FileT, is: InputStream): Box[(DicomEntities, DicomImportData)] = {
    readEntities(file, is).map { entities =>
      trace("Storing DICOM records")

      Dicom.synchronized {
        Slick.db.withDynTransaction {
          val (patient, isNewPatient) = readOrSavePatient {
            entities.patient.copy(userId = file.userId)
          }

          val (study, isNewStudy) = readOrSaveStudy {
            entities.study.copy(patientId = patient.id)
          }

          val (series, isNewSeries) = readOrSaveSeries {
            entities.series.copy(studyId = study.id)
          }

          val (image, isNewImage) = readOrSaveImage {
            entities.image.copy(fileId = file.id, seriesId = series.id)
          }

          Tuple2(
            DicomEntities(patient, study, series, image)
          , DicomImportData(isNewPatient, isNewStudy, isNewSeries, isNewImage)
          )
        }
      }
    }
  }

  def readOrSavePatient(patient: PatientT): (PatientT, Boolean) = {
    Patients.findExisting(patient).map { patient =>
      (patient, false)
    } getOrElse {
      (Patients.insert(patient), true)
    }
  }

  def readOrSaveStudy(study: StudyT): (StudyT, Boolean) = {
    Studies.findExisting(study).map { study =>
      (study, false)
    } getOrElse {
      (Studies.insert(study), true)
    }
  }

  def readOrSaveSeries(series: SeriesT): (SeriesT, Boolean) = {
    Seriess.findExisting(series).map { series =>
      (series, false)
    } getOrElse {
      (Seriess.insert(series), true)
    }
  }

  def readOrSaveImage(image: ImageT): (ImageT, Boolean) = {
    Images.findExisting(image).map { image =>
      (image, false)
    } getOrElse {
      (Images.insert(image), true)
    }
  }

  def readTags(is: InputStream): Seq[TagValue] = {
    val attrs = new DicomInputStream(is).readDataset(-1, -1)
    val tags = attrs.tags()
    val dict = StandardElementDictionary.INSTANCE

    (0 until attrs.size()).map { index =>
      val tag = tags(index)
      TagValue(
        TagUtils.groupNumber(tag),
        TagUtils.elementNumber(tag),
        dict.keywordOf(tag),
        dict.vrOf(tag).toString,
        attrs.getString(tag, ""))
    }
  }

  def frameData(is: InputStream, frame: Int = 0): Box[BufferedImage]= {
    ImageIO.getImageReadersByFormatName("DICOM")
           .foldLeft[Box[BufferedImage]](Empty) { (result, ir) =>
      result or tryo {
        val iis = ImageIO.createImageInputStream(is)
        ir.setInput(iis)
        ir.read(0)
      }
    }
  }

  def getDateTime(attrs: Attributes, dateTag: Int, timeTag: Int): DateTime = {
    val result = new DateTime

    val date = new GregorianCalendar()
    date.setTime(attrs.getDate(dateTag))
    date.setTimeZone(attrs.getTimeZone)

    val time = new GregorianCalendar()
    time.setTime(attrs.getDate(timeTag))
    time.setTimeZone(attrs.getTimeZone)

    new DateTime().withDate(
      date.get(Calendar.YEAR)
    , date.get(Calendar.MONTH) + 1
    , date.get(Calendar.DAY_OF_MONTH)
    ).withTime(
      time.get(Calendar.HOUR)
    , time.get(Calendar.MINUTE)
    , time.get(Calendar.SECOND)
    , time.get(Calendar.MILLISECOND)
    ).withHourOfDay(
      time.get(Calendar.HOUR)
    ).withZoneRetainFields(DateTimeZone.forTimeZone(attrs.getTimeZone))
  }

  def boot = {
    val factory = new ImageReaderFactory
    factory.load("dcm4che_ImageReaderFactory.properties")
    ImageReaderFactory.setDefault(factory);
  }
}
