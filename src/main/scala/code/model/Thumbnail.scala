package code.model

import code.lib._

import scala.concurrent._

import java.io.InputStream

import org.joda.time.DateTime

import net.liftweb.util.Helpers._
import net.liftweb.common._

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import Slick._

case class ThumbnailId(val value: Long) extends MappedTo[Long] with GenericId

object ThumbnailId {
  val empty = ThumbnailId(-1)
}

case class ThumbnailT(
  dimension: Int
, size: Long
, key: String
, imageId: ImageId
, createdAt: DateTime = DateTime.now
, id: ThumbnailId = ThumbnailId.empty
) extends BlobT

class ThumbnailsTable(tag: Tag) extends Table[ThumbnailT](tag, "thumbnails") {
  def dimension = column[Int]("dimension")
  def size = column[Long]("size")
  def key = column[String]("key")
  def imageId = column[ImageId]("image_id")
  def createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))

  def id = column[ThumbnailId]("id", O.PrimaryKey, O.AutoInc)

  def image = foreignKey("thumbnails_image_id__fk", imageId, Images)(_.id,
      onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (dimension, size, key, imageId, createdAt, id) <>
          (ThumbnailT.tupled, ThumbnailT.unapply)
}

object Thumbnails extends TableQuery(new ThumbnailsTable(_)) with Loggable {
  val forInsert = Thumbnails.returning(map(_.id))

  def insert(thumbnail: ThumbnailT) = {
    val id = forInsert.insert(thumbnail)
    thumbnail.copy(id = id)
  }

  def delete_!(id: ThumbnailId) = Thumbnails.filter(_.id === id).delete

  val byId = for {
    id <- Parameters[ThumbnailId]
    i <- Thumbnails if i.id === id
  } yield i

  def find(id: ThumbnailId): Option[ThumbnailT] = byId(id).firstOption

  val byImageId = for {
    imageId <- Parameters[ImageId]
    t <- Thumbnails if t.imageId === imageId
  } yield t

  def findByImageId(imageId: ImageId): List[ThumbnailT] = byImageId(imageId).list

  val bySeriesId = for {
    seriesId <- Parameters[SeriesId]
    i <- Images if i.seriesId === seriesId
    t <- Thumbnails if t.imageId === i.id
  } yield t

  def findBySeriesId(seriesId: SeriesId): List[ThumbnailT] = bySeriesId(seriesId).list

  val access = for {
    (thumbnailId, userId) <- Parameters[(ThumbnailId, UserId)]
    thumb <- Thumbnails if thumb.id === thumbnailId
    img <- thumb.image if !img.isRemoved
    ser <- img.series if !ser.isRemoved
    stu <- ser.study if !stu.isRemoved
    pat <- stu.patient if !pat.isRemoved
    user <- pat.user if user.id === userId && user.active
  } yield user.id

  def canAccess(thumbnail: ThumbnailT, user: UserT): Boolean = {
    access(thumbnail.id, user.id).firstOption == Some(user.id)
  }

  def largestByImageId(imageId: ImageId) = (for {
    t <- Thumbnails if t.imageId === imageId
  } yield t).sortBy(_.dimension.desc).firstOption

  val minThumbnailDimension = 128

  def dimensions(max: Int): List[Int] = {
    Stream.iterate(minThumbnailDimension)(_ * 2).takeWhile(_ < max).toList :+ max
  }
}
