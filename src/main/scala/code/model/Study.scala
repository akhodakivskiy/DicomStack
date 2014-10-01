package code.model

import code.lib._

import org.joda.time.{DateTime, DateTimeZone}

import org.dcm4che3.data.{Tag => DicomTag, Attributes}

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import Slick._

case class StudyId(val value: Long) extends MappedTo[Long] with GenericId

object StudyId {
  val empty = new StudyId(-1)
}

case class StudyT(
  patientId: PatientId
, instanceUid: String
, description: String
, date: DateTime
, modality: String
, isRemoved: Boolean = false
, id: StudyId = StudyId.empty
) {
  def descriptionNode = Util.maybeUnnamed(description, Util.unnamedStudy)
}

class StudiesTable(tag: Tag) extends Table[StudyT](tag, "studies") {
  def patientId = column[PatientId]("patient_id")
  def instanceUid = column[String]("instance_uid")
  def description = column[String]("description")
  def date = column[DateTime]("date", O.DBType("TIMESTAMP WITH TIME ZONE"))
  def modality = column[String]("modality")
  def isRemoved = column[Boolean]("is_removed")

  def id = column[StudyId]("id", O.PrimaryKey, O.AutoInc)

  def descriptionTs = column[TSVector]("description_ts", O.DBType("TSVECTOR"))

  def patient = foreignKey("studies_patient_id__fk", patientId, Patients)(_.id,
      onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (patientId, instanceUid, description, date, modality, isRemoved, id) <>
          (StudyT.tupled, StudyT.unapply)
}

object Studies extends TableQuery(new StudiesTable(_)) {
  val forInsert = Studies.returning(map(_.id))

  def insert(study: StudyT) = {
    val id = forInsert.insert(study)
    study.copy(id = id)
  }

  val byId = for {
    id <- Parameters[StudyId]
    s <- Studies if s.id === id && !s.isRemoved
  } yield s

  def find(id: StudyId): Option[StudyT] = byId(id).firstOption

  val byPatientId = for {
    id <- Parameters[PatientId]
    s <- Studies if s.patientId === id && !s.isRemoved
  } yield s

  def findByPatientId(patientId: PatientId) = byPatientId(patientId).list

  val existing = for {
    (patientId, instanceUid) <- Parameters[(PatientId, String)]
    s <- Studies if s.patientId === patientId && !s.isRemoved &&
                    s.instanceUid === instanceUid
  } yield s

  def findExisting(s: StudyT) = existing(s.patientId, s.instanceUid).firstOption

  val access = for {
    (studyId, userId) <- Parameters[(StudyId, UserId)]
    stu <- Studies if stu.id === studyId && !stu.isRemoved
    pat <- stu.patient if !pat.isRemoved
    user <- pat.user if user.id === userId && user.active
  } yield stu.id

  def canAccess(study: StudyT, user: UserT): Boolean = {
    access(study.id, user.id).firstOption == Some(study.id)
  }

  def fromAttrs(attrs: Attributes) = StudyT(
    PatientId.empty
  , attrs.getString(DicomTag.StudyInstanceUID, "")
  , attrs.getString(DicomTag.StudyDescription, "")
  , Dicom.getDateTime(attrs, DicomTag.StudyDate, DicomTag.StudyTime)
  , attrs.getString(DicomTag.Modality, "")
  )
}
