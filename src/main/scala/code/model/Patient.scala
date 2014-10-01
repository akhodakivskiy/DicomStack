package code.model

import org.joda.time.{DateTime, DateTimeZone}
import org.dcm4che3.data.{PersonName, Tag => DicomTag, Attributes}

import net.liftweb.common._
import net.liftweb.util.Helpers.{capify}

import code.lib._

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import Slick._

object Sex extends Enumeration {
  case class Sex(name: String) extends Val(name) {
    override def toString = name.capitalize
  }
  val Male = Sex("male")
  val Female = Sex("female")
  val Other = Sex("other")

  def findOption(s: String) : Option[Sex] = List(Male, Female, Other).find (_.name.head.toLower == s.head.toLower)
  def find(s: String, default: Sex = Other) : Sex = findOption(s) getOrElse default

  implicit val userTokenColumnType = {
    MappedColumnType.base[Sex, String](
      sex => sex.name,
      str => find(str, Other))
  }
}
import Sex._

case class PatientId(val value: Long) extends MappedTo[Long] with GenericId

object PatientId {
  val empty = new PatientId(-1)
}

case class PatientT(
  userId: UserId
, name: String
, identifier: String
, identifierIssuer: String
, otherIdentifiers: String
, birthDate: DateTime
, address: String
, phone: String
, sex: Sex = Other
, isRemoved: Boolean = false
, id: PatientId = PatientId.empty
) {
  def fullName = Patients.fullName(name)
  def fullNameNode = Util.maybeUnnamed(fullName, Util.unnamedPatient)
}

class PatientsTable(tag: Tag) extends Table[PatientT](tag, "patients") {
  def userId = column[UserId]("user_id")
  def name = column[String]("name")
  def identifier = column[String]("identifier")
  def identifierIssuer = column[String]("identifier_issuer")
  def otherIdentifiers = column[String]("other_identifiers")
  def birthDate = column[DateTime]("birth_date", O.DBType("TIMESTAMP WITH TIME ZONE"))
  def address = column[String]("address")
  def phone = column[String]("phone")
  def sex = column[Sex]("sex")
  def isRemoved = column[Boolean]("is_removed")

  def id = column[PatientId]("id", O.PrimaryKey, O.AutoInc)

  def nameTs = column[TSVector]("name_ts", O.DBType("TSVECTOR"))

  def user = foreignKey("patients_user_id__fk", userId, Users)(_.id,
      onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (userId, name, identifier, identifierIssuer, otherIdentifiers, birthDate,
            address, phone, sex, isRemoved, id) <>
          (PatientT.tupled, PatientT.unapply)
}

object Patients extends TableQuery(new PatientsTable(_)) {
  val forInsert = Patients.returning(map(_.id))

  def insert(patient: PatientT) = {
    val id = forInsert.insert(patient)
    patient.copy(id = id)
  }

  val byId = for {
    id <- Parameters[PatientId]
    p <- Patients if p.id === id && !p.isRemoved
  } yield p

  def find(id: PatientId): Option[PatientT] = byId(id).firstOption

  val existing = for {
    (userId, identifier, identifierIssuer) <- Parameters[(UserId, String, String)]
    p <- Patients if p.userId === userId && !p.isRemoved &&
                     p.identifier === identifier &&
                     p.identifierIssuer === identifierIssuer
  } yield p

  def findExisting(p: PatientT) = {
    existing(p.userId, p.identifier, p.identifierIssuer).firstOption
  }

  def fullName(dicomName: String): String = {
    val personName = new PersonName(dicomName)
    List(
      PersonName.Component.NamePrefix
    , PersonName.Component.FamilyName
    , PersonName.Component.MiddleName
    , PersonName.Component.GivenName
    , PersonName.Component.NameSuffix
    ) .map(s => personName.get(s))
      .filter(s => s != null && s.length > 0)
      .map(s => capify(s))
      .mkString(" ")
  }

  def fromAttrs(attrs: Attributes): PatientT = PatientT(
    UserId.empty
  , attrs.getString(DicomTag.PatientName, "")
  , attrs.getString(DicomTag.PatientID, "")
  , attrs.getString(DicomTag.IssuerOfPatientID, "")
  , attrs.getString(DicomTag.OtherPatientIDs, "")
  , Dicom.getDateTime(attrs, DicomTag.PatientBirthDate, DicomTag.PatientBirthDate)
  , attrs.getString(DicomTag.PatientAddress, "")
  , attrs.getString(DicomTag.PatientTelephoneNumbers, "")
  , Sex.find(attrs.getString(DicomTag.PatientSex, ""))
  )
}
