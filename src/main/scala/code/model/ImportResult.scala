package code.model

import code.lib._

import net.liftweb.json._

import org.joda.time.DateTime

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import Slick._

object ImportKind extends Enumeration {
  case class ImportKind(name: String)
  val DicomKind = ImportKind("dicom")
  val DicomNoImageKind = ImportKind("dicom_no_image")
  val IgnoreKind = ImportKind("ignore")
  val FailureKind = ImportKind("failure")
  val UnzipKind = ImportKind("unzip")

  def findOption(s: String): Option[ImportKind] = {
    List(DicomKind, DicomNoImageKind, IgnoreKind, FailureKind, UnzipKind).find(_.name == s)
  }

  def find(s: String): ImportKind = findOption(s).getOrElse(sys.error("unknown import kind"))

  implicit def importKindColumnType = {
    MappedColumnType.base[ImportKind, String](_.name, find)
  }
}
import ImportKind._

case class ImportResultId(value: Long) extends MappedTo[Long]

object ImportResultId {
  val empty = ImportResultId(-1)
}

case class ImportResultT(
  fileId: FileId
, kind: ImportKind
, version: Int
, error: Option[ThrowableT] = None
, patientId: Option[PatientId] = None
, studyId: Option[StudyId] = None
, seriesId: Option[SeriesId] = None
, imageId: Option[ImageId] = None
, createdAt: DateTime = DateTime.now
, id: ImportResultId = ImportResultId.empty
)

class ImportResultsTable(tag: Tag) extends Table[ImportResultT](tag, "import_results") {
  def fileId = column[FileId]("file_id")
  def kind = column[ImportKind]("kind")
  def version = column[Int]("version")
  def error = column[Option[ThrowableT]]("error", O.DBType("TEXT"))
  def patientId = column[Option[PatientId]]("patient_id")
  def studyId = column[Option[StudyId]]("study_id")
  def seriesId = column[Option[SeriesId]]("series_id")
  def imageId = column[Option[ImageId]]("image_id")
  def createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))

  def id = column[ImportResultId]("id", O.PrimaryKey, O.AutoInc)

  def fileId_idx = index("import_results_file_id__idx", fileId)

  def file = foreignKey("import_results_file_id__fk", fileId, Files)(_.id,
      onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (fileId, kind, version, error,
           patientId, studyId, seriesId, imageId, createdAt, id) <>
          (ImportResultT.tupled, ImportResultT.unapply)
}

object ImportResults extends TableQuery(new ImportResultsTable(_)) {
  val forInsert = this.returning(map(_.id))

  def insert(result: ImportResultT) = {
    val id = forInsert.insert(result)
    result.copy(id = id)
  }

  def apply(fileId: FileId, kind: ImportKind): ImportResultT = {
    ImportResultT(fileId, kind, 0)
  }
  def apply(fileId: FileId): ImportResultT = {
    apply(fileId, IgnoreKind)
  }
  def apply(fileId: FileId, t: Option[Throwable]): ImportResultT = {
    apply(fileId, FailureKind).copy(error = t.map(ThrowableT.apply))
  }
  def apply(fileId: FileId, entities: DicomEntities, imageRead: Boolean): ImportResultT = {
    apply(fileId, if (imageRead) DicomKind else DicomNoImageKind)
      .copy(patientId = Some(entities.patient.id))
      .copy(studyId = Some(entities.study.id))
      .copy(seriesId = Some(entities.series.id))
      .copy(imageId = Some(entities.image.id))
  }
}
