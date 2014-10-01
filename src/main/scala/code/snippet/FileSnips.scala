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
import net.liftweb.http.js.jquery._
import net.liftweb.util._
import net.liftweb.util.Helpers._

class FileName(file: FileT) {
  def render = "* *" #> file.name
}

object FilesPage extends Loggable {
  import scala.slick.driver.PostgresDriver.simple._
  import Database.dynamicSession
  import Slick._

  def byUserAndParent(userId: UserId, parentIdOpt: Option[FileId]) = {
    parentIdOpt.map { parentId =>
      Files.filter(_.userId === userId).filter(!_.isRemoved).filter(_.parentId === parentId)
    } getOrElse {
      Files.filter(_.userId === userId).filter(!_.isRemoved).filter(_.parentId.isEmpty)
    }
  }

  def count(userId: UserId, parentId: Option[FileId]) = {
    byUserAndParent(userId, parentId).length.run
  }

  def page(userId: UserId, parentId: Option[FileId], size: Long, page: Long,
                  sort: String, asc: Boolean) = {
    byUserAndParent(userId, parentId)
      .sortBy { a =>
        val col = sort match {
          case "name" => a.name
          case "size" => a.size
          case "date" => a.createdAt
        }
        if (asc) col.asc else col.desc
      }.drop(page * size).take(size).list
  }

  def findArchiveSizes(fileIds: Seq[FileId]) = (for {
    f <- Files if f.parentId inSet fileIds
  } yield f).groupBy(_.parentId).map {
    case (fileId, fcs) => (fileId, fcs.length)
  }.list

  def findImportVersions(fileIds: Seq[FileId]) = (for {
    r <- ImportResults if ((r.fileId inSet fileIds) && (r.kind =!= ImportKind.FailureKind))
  } yield r).groupBy(_.fileId).map {
    case (fileId, fcs) => (fileId, fcs.map(_.version).max)
  }.list

  def remove(files: List[FileT]) = {
    Files.filter(_.id inSet files.map(_.id)).map(_.isRemoved).update(true)
  }

  def undoRemove(files: List[FileT]) = {
    Files.filter(_.id inSet files.map(_.id)).map(_.isRemoved).update(false)
  }

  object selectionList extends SessionVar[List[FileT]](Nil)
  object undoList extends SessionVar[List[FileT]](Nil)

  object removeButtonId extends RequestVar[String]("")
}

class FilesPage(
  parent: Option[FileT]
) extends BootstrapSortedPaginatorSnippet[FileT, String] with Loggable {
  def this() = this(None)
  def this(file: FileT) = this(Some(file))

  val user = ExtSessions.user.openOrThrowException("must be authenticated")

  override def itemsPerPage = 15

  override val count = FilesPage.count(user.id, parent.map(_.id)).toLong

  override def page = sort match { case (idx, asc) =>
    val header = headers(idx)._1
    FilesPage.page(user.id, parent.map(_.id), itemsPerPage, curPage, header, asc)
  }

  override def headers = List("name", "size", "date").map(c => (c, c))
  sort_=((2, false)) // default sort by date desc

  def render = {
    val files = page
    val archiveSizes = FilesPage.findArchiveSizes(files.map(_.id))
    val versions: List[(FileId, Option[Int])] = FilesPage.findImportVersions(files.map(_.id))
    val filesWithData = files.map { file =>
      val isArchive = archiveSizes.find(_._1 == Some(file.id)).map(_._2 > 0).getOrElse(false)
      (file, isArchive)
    }.map { case (file, isArchive) =>
      val vOpt = versions.find(_._1 == file.id).flatMap(_._2)
      (file, isArchive, vOpt)
    }
    "tr" #> filesWithData.map { case (file, isArchive, versionOpt) =>
      renderRow(file, isArchive, versionOpt, nextFuncName)
    }
  }

  def renderRow(file: FileT, isArchive: Boolean, version: Option[Int], rowId: String) = {
    "tr [id]" #> rowId &
    "role=filename"       #> {
      "a *"       #> Util.elideMiddle(file.name, 30) &
      "a [title]" #> file.name
    } &
    "role=length *"       #> Util.humanReadableByteCount(file.size) &
    "role=uploadDate *"   #> Util.DTF.localMedium.print(file.createdAt) &
    "role=decompose [onclick]" #> SHtml.ajaxInvoke(() => {
      Defer.importFuture(file)
      JqJsCmds.EmptyAfter("messages", NodeSeq.Empty) &
      MyJsCmds.JqNotice("messages", Text(s"Importing file ${file.name}"))
    }) &
    "role=tags [href]" #> Site.files.tags.calcHref(file) &
    "role=archive" #> (if (isArchive) PassThru else ClearNodes) &
    "role=archive [href]" #> Site.files.archive.calcHref(file) &
    "role=version" #> (if (version.isDefined) PassThru else ClearNodes) &
    "role=version *" #> version.map(_.toString).getOrElse("") &
    "role=remove" #> {
      "type=checkbox" #> SHtml.ajaxCheckbox(false, (value) => {
        FilesRemove.selectionList.update { list =>
          if (value) (file :: list) else (list.filter(_ != file))
        }
        val count = FilesRemove.selectionList.get.length
        val disabled = if (count == 0) JE.Str("disabled") else JE.JsNull
        (JqJE.JqId(FilesRemove.removeButtonId.get) ~> JqJE.JqAttr("disabled", disabled)).cmd
      })
    }
  }
}

object FilesRemove extends Loggable {
  object selectionList extends SessionVar[List[FileT]](Nil)
  object undoList extends SessionVar[List[FileT]](Nil)

  object removeButtonId extends RequestVar[String]("")

  def render = {
    removeButtonId.set(nextFuncName)

    def remove() = {
      val count = selectionList.get.length
      FilesPage.remove(selectionList.get)
      undoList(selectionList.get)
      selectionList(Nil)
      S.notice(Text(s"Removed $count studies."))
      S.seeOther(S.uri)
    }
    def undo() = {
      val count = undoList.get.length
      FilesPage.undoRemove(undoList.get)
      undoList(Nil)
      S.notice(Text(s"Restored $count studies."))
      S.seeOther(S.uri)
    }

    "role=remove [id]" #> removeButtonId.get &
    "role=remove [disabled]" #> "disabled" &
    "role=remove" #> SHtml.onSubmitUnit(remove) &
    "role=undo [disabled]" #> (if (undoList.get.length == 0) Full("disabled") else Empty) &
    "role=undo" #> SHtml.onSubmitUnit(undo)
  }
}
