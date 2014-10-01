package code.model

import code.lib._

import org.joda.time.{DateTime, DateTimeZone}

import net.liftweb.common._

import org.dcm4che3.data.{Tag => DicomTag, Attributes}

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import Slick._

case class SeriesId(val value: Long) extends MappedTo[Long] with GenericId

object SeriesId {
  val empty = new SeriesId(-1)
}

case class SeriesT(
  studyId: StudyId
, instanceUid: String
, description: String
, date: DateTime
, number: Int
, modality: String
, isRemoved: Boolean = false
, id: SeriesId = SeriesId.empty
)

class SeriesTable(tag: Tag) extends Table[SeriesT](tag, "series") {
  def studyId = column[StudyId]("study_id")
  def instanceUid = column[String]("instance_uid")
  def description = column[String]("description")
  def date = column[DateTime]("date", O.DBType("TIMESTAMP WITH TIME ZONE"))
  def number = column[Int]("number")
  def modality = column[String]("modality")
  def isRemoved = column[Boolean]("is_removed")

  def id = column[SeriesId]("id", O.PrimaryKey, O.AutoInc)

  def study = foreignKey("series_study_id__fk", studyId, Studies)(_.id,
      onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (studyId, instanceUid, description, date, number, modality, isRemoved, id) <>
          (SeriesT.tupled, SeriesT.unapply)
}

object Seriess extends TableQuery(new SeriesTable(_)) {
  val forInsert = Seriess.returning(map(_.id))

  def insert(series: SeriesT) = {
    val id = forInsert.insert(series)
    series.copy(id = id)
  }

  val byId = for {
    id <- Parameters[SeriesId]
    s <- Seriess if s.id === id && !s.isRemoved
  } yield s

  def find(id: SeriesId): Option[SeriesT] = byId(id).firstOption

  val byStudyId = for {
    studyId <- Parameters[StudyId]
    s <- Seriess if s.studyId === studyId && !s.isRemoved
  } yield s

  def findByStudyId(studyId: StudyId) = byStudyId(studyId).list
  def firstByStudyId(studyId: StudyId) = byStudyId(studyId).firstOption

  val existing = for {
    (id, instanceUid) <- Parameters[(StudyId, String)]
    s <- Seriess if s.studyId === id && !s.isRemoved &&
                  s.instanceUid === instanceUid
  } yield s

  def findExisting(s: SeriesT) = existing(s.studyId, s.instanceUid).firstOption

  val access = for {
    (seriesId, userId) <- Parameters[(SeriesId, UserId)]
    ser <- Seriess if ser.id === seriesId && !ser.isRemoved
    stu <- ser.study if !stu.isRemoved
    pat <- stu.patient if !pat.isRemoved
    user <- pat.user if user.id === userId && user.active
  } yield user.id

  def canAccess(series: SeriesT, user: UserT): Boolean = {
    access(series.id, user.id).firstOption == Some(user.id)
  }

  def fromAttrs(attrs: Attributes): SeriesT = {
    val date = if (attrs.contains(DicomTag.SeriesDate))
      Dicom.getDateTime(attrs, DicomTag.SeriesDate, DicomTag.SeriesTime)
    else
      Dicom.getDateTime(attrs, DicomTag.StudyDate, DicomTag.StudyTime)

    SeriesT(
      StudyId.empty
    , attrs.getString(DicomTag.SeriesInstanceUID)
    , attrs.getString(DicomTag.SeriesDescription, "")
    , date
    , attrs.getInt(DicomTag.SeriesNumber, 0)
    , attrs.getString(DicomTag.Modality, "")
    )
  }
}
