package code.model

import code.lib._

import org.dcm4che3.data.{Tag => DicomTag, Attributes}

import net.liftweb.common._
import net.liftweb.json._

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import Slick._

case class ImageId(val value: Long) extends MappedTo[Long] with GenericId

object ImageId {
  val empty = new ImageId(-1)
}

case class ImageT(
  seriesId: SeriesId
, fileId: FileId
, sopInstanceUid: String
, transferSyntax: String
, instanceNumber: Int
, rows: Int
, columns: Int
, isRemoved: Boolean = false
, id: ImageId = ImageId.empty
)

class ImagesTable(tag: Tag) extends Table[ImageT](tag, "images") {
  def seriesId = column[SeriesId]("series_id")
  def fileId = column[FileId]("file_id")
  def sopInstanceUid = column[String]("sop_instance_uid")
  def transferSyntax = column[String]("transfer_syntax")
  def instanceNumber = column[Int]("instance_number")
  def rows = column[Int]("rows")
  def columns = column[Int]("columns")
  def isRemoved = column[Boolean]("is_removed")

  def id = column[ImageId]("id", O.PrimaryKey, O.AutoInc)

  def series = foreignKey("images_series_id__fk", seriesId, Seriess)(_.id,
      onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def file = foreignKey("images_file_id__fk", fileId, Files)(_.id,
      onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (seriesId, fileId, sopInstanceUid, transferSyntax, instanceNumber,
            rows, columns, isRemoved, id) <>
          (ImageT.tupled, ImageT.unapply)
}

object Images extends TableQuery(new ImagesTable(_)) {
  val forInsert = Images.returning(map(_.id))

  def insert(image: ImageT) = {
    val id = forInsert.insert(image)
    image.copy(id = id)
  }

  val byId = for {
    id <- Parameters[ImageId]
    i <- Images if i.id === id && !i.isRemoved
  } yield i

  def find(id: ImageId): Option[ImageT] = byId(id).firstOption

  val bySeriesId = for {
    seriesId <- Parameters[SeriesId]
    i <- Images if i.seriesId === seriesId && !i.isRemoved
  } yield i

  def findBySeriesId(seriesId: SeriesId): List[ImageT] = bySeriesId(seriesId).list

  val dicomEntities= for {
    imageId <- Parameters[ImageId]
    img <- Images if img.id === imageId && !img.isRemoved
    ser <- img.series if !ser.isRemoved
    stu <- ser.study if !stu.isRemoved
    pat <- stu.patient if !pat.isRemoved
  } yield (img, ser, stu, pat)

  def findDicomEntities(imageId: ImageId): Option[(ImageT, SeriesT, StudyT, PatientT)] = {
    dicomEntities(imageId).firstOption
  }

  val existing = for {
    (seriesId, sopInstanceUid) <- Parameters[(SeriesId, String)]
    i <- Images if i.seriesId === seriesId && !i.isRemoved &&
                  i.sopInstanceUid === sopInstanceUid
  } yield i

  def findExisting(i: ImageT) = existing(i.seriesId, i.sopInstanceUid).firstOption

  val access = for {
    (imageId, userId) <- Parameters[(ImageId, UserId)]
    img <- Images if img.id === imageId && !img.isRemoved
    ser <- img.series if !ser.isRemoved
    stu <- ser.study if !stu.isRemoved
    pat <- stu.patient if !pat.isRemoved
    user <- pat.user if user.id === userId && user.active
  } yield user.id

  def canAccess(image: ImageT, user: UserT): Boolean = {
    access(image.id, user.id).firstOption == Some(user.id)
  }

  def fromAttrs(attrs: Attributes, ts: String) = {
    ImageT(
      SeriesId.empty
    , FileId.empty
    , attrs.getString(DicomTag.SOPInstanceUID)
    , ts
    , attrs.getInt(DicomTag.InstanceNumber, -1)
    , attrs.getInt(DicomTag.Rows, -1)
    , attrs.getInt(DicomTag.Columns, -1)
    )
  }
}
