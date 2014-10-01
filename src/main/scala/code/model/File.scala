package code.model

import code.lib._
import org.joda.time.DateTime

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import code.lib.Slick._

trait BlobT {
  def size: Long
  def key: String
  def createdAt: DateTime
}

case class FileId(val value: Long) extends MappedTo[Long] with GenericId

object FileId {
  val empty = new FileId(-1)
}

case class FileT(
  userId: UserId
, name: String
, size: Long
, key: String
, parentId: Option[FileId] = None
, isRemoved : Boolean = false
, createdAt: DateTime = DateTime.now
, id: FileId = FileId.empty
) extends BlobT

class FilesTable(tag: Tag) extends Table[FileT](tag, "files") {
  def userId = column[UserId]("user_id")
  def name = column[String]("name")
  def size = column[Long]("size")
  def key = column[String]("key")
  def parentId = column[Option[FileId]]("parent_id")
  def createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  def isRemoved = column[Boolean]("is_removed")

  def id = column[FileId]("id", O.PrimaryKey, O.AutoInc)

  def user = foreignKey("files_user_id__fk", userId, Users)(_.id,
      onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)

  def * = (userId, name, size, key, parentId, isRemoved, createdAt, id) <>
          (FileT.tupled, FileT.unapply)
}

object Files extends TableQuery(new FilesTable(_)) {
  val queryReturningId = Files.returning(map(_.id))

  def insert(file: FileT) = {
    val id = queryReturningId.insert(file)
    file.copy(id = id)
  }

  def update(file: FileT) = filter(_.id === file.id).update(file)

  def remove(id: FileId) = {
    filter(_.id === id).filter(!_.isRemoved).map(_.isRemoved).update(true)
  }

  def delete_!(id: FileId) = {
    filter(_.id === id).delete
  }

  val byId = for {
    id <- Parameters[FileId]
    f <- Files if f.id === id && !f.isRemoved
  } yield f

  def find(fileId: FileId): Option[FileT] = byId(fileId).firstOption

  val archive = Compiled { (fileId: Column[FileId]) =>
    for {
      (f, c) <- Files innerJoin Files on (_.id === _.parentId) if
                f.id === fileId && !f.isRemoved && !c.isRemoved
    } yield f
  }

  // Returns file if it has children files, e.g. archive
  def findArchive(fileId: FileId): Option[FileT] = archive(fileId).firstOption
}
