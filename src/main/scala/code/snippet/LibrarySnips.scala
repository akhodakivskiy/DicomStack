package code.snippet

import code.model._
import code.lib._

import java.io.InputStream

import net.liftweb.actor._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.jquery._
import net.liftweb.util._
import net.liftweb.util.Helpers._

import scala.concurrent._
import scala.xml._

import scala.slick.driver.PostgresDriver.simple._
import Database._
import Slick._

object LibraryPage {
  import scala.slick.driver.PostgresDriver.simple._
  import Database.dynamicSession
  import Slick._

  def pws(userId: UserId) = for {
    p <- Patients if p.userId === userId && !p.isRemoved
    s <- Studies if s.patientId === p.id && !s.isRemoved
  } yield (p, s)

  def pwsQuery(userId: UserId, q: TextQuery) = {
    (for {
      p <- Patients if p.userId === userId && !p.isRemoved
      s <- Studies if s.patientId === p.id && !s.isRemoved
    } yield (p, s)).filter { case (p, s) =>
      (p.nameTs @| s.descriptionTs) @@ tsQuery(q.query)
    }
  }

  def patientsWithStudies(userId: UserId, qBox: Option[String]) = {
    qBox.flatMap(TextQuery.apply) match {
      case Some(q) => pwsQuery(userId, q)
      case None => pws(userId)
    }
  }

  def patientsWithStudiesCount(userId: UserId, q: Option[String]) = {
    patientsWithStudies(userId, q).length.run
  }

  def patientsWithStudiesPage(userId: UserId, q: Option[String], size: Int, page: Int,
           sort: String, asc: Boolean): List[(PatientT, StudyT)] = {
    patientsWithStudies(userId, q)
      .sortBy { a =>
        val col = sort match {
          case "patient" => a._1.name
          case "dob" => a._1.birthDate
          case "study" => a._2.description
          case "date" => a._2.date
        }
        if (asc) col.asc else col.desc
      }.drop(page * size).take(size).list
  }

  // Change isRemoved status of the study, series, and imagares
  private def _remove(studyId: StudyId, remove: Boolean) = {
    Studies.filter(_.id === studyId).filter(_.isRemoved =!= remove)
           .map(_.isRemoved).update(remove)

    Seriess.filter(_.studyId === studyId).filter(_.isRemoved =!= remove)
           .map(_.isRemoved).update(remove)

    val imageIds = (for {
      s <- Seriess if s.studyId === studyId
      i <- Images if i.seriesId === s.id
    } yield i.id).list

    Images.filter(i => (i.id inSet imageIds)).map(_.isRemoved).update(remove)
  }

  // Remove study, empty the selection list and fill the undo list
  def remove() = Slick.db.withDynTransaction {
    undoList(selectionList.get)

    selectionList.foreach { case (patient, study) =>
      _remove(study.id, true)

      if (Studies.filter(_.patientId === patient.id).filter(!_.isRemoved).length.run == 0) {
        Patients.filter(_.id === patient.id).filter(!_.isRemoved).map(_.isRemoved).update(true)
      }
    }
    selectionList(Nil)
  }

  // Undo study removal, empty the selection undo list
  def undoRemove() = Slick.db.withDynTransaction {
    undoList.foreach { case (patient, study) =>
      _remove(study.id, false)

      Patients.filter(_.id === patient.id).filter(_.isRemoved).map(_.isRemoved).update(false)
    }
    undoList(Nil)
  }

  object selectionList extends SessionVar[List[(PatientT, StudyT)]](Nil)
  object undoList extends SessionVar[List[(PatientT, StudyT)]](Nil)

  object removeButtonId extends RequestVar[String]("")

  object pageMemo extends RequestVar[Box[IdMemoizeTransform]](Empty)
}

class LibraryPage extends BootstrapSortedPaginatorSnippet[(PatientT, StudyT), String]
                  with Loggable {
  val user = ExtSessions.user.openOrThrowException("must be authenticated")

  val query = S.param("q")

  override def sortedPageUrl(offset: Long, sort: (Int, Boolean)): String = {
    val url = super.sortedPageUrl(offset, sort)
    val params = query.map(q => List("q" -> q)).openOr(Nil)
    appendParams(url, params)
  }

  override val count = {
    LibraryPage.patientsWithStudiesCount(user.id, query).toLong
  }
  override def page = sort match { case (idx, asc) =>
    val header = headers(idx)._1
    LibraryPage.patientsWithStudiesPage(user.id, query, itemsPerPage, curPage, header, asc)
  }
  override def headers = List("patient", "dob", "study", "date").map(c => (c, c))

  private def renderStudy(patient: PatientT)(study: StudyT) = {
    "role=studyDescription" #> {
      "a *" #> study.descriptionNode &
      "a [href]" #> Site.library.study.calcHref(study)
    } &
    "role=studyDate *" #> Util.DF.localMedium.print(study.date) &
    "role=studyDate [title]" #> Util.DTF.localMedium.print(study.date) &
    "role=remove" #> {
      "type=checkbox" #> SHtml.ajaxCheckbox(false, (v) => {
        LibraryPage.selectionList.update { list =>
          val pair = (patient, study)
          if (v) {
            pair :: list
          } else {
            list.filter(_ != pair)
          }
        }
        val count = LibraryPage.selectionList.get.length
        val disabled = if (count == 0) JE.Str("disabled") else JE.JsNull
        (JqJE.JqId(LibraryPage.removeButtonId.get) ~> JqJE.JqAttr("disabled", disabled)).cmd
      })
    }
  }

  private def renderPatient(patient: PatientT, studies: Seq[StudyT]) = {
    "role=firstRow" #> {
      "role=patientSex [rowspan]" #> studies.length &
      "role=patientName" #> {
        "a *" #> patient.fullNameNode &
        "a [title]" #> patient.name &
        "a [href]" #> Site.library.patient.calcHref(patient) &
        "span *" #> patient.sex.toString
      } &
      "role=patientDob [rowspan]" #> studies.length &
      "role=patientDob *" #> Util.DF.localMedium.print(patient.birthDate) &
      "role=patientDob [title]" #> Util.DTF.localMedium.print(patient.birthDate) &
      "role=patientName [rowspan]" #> studies.length &
      renderStudy(patient)(studies.head)
    } &
    "role=followingRow" #> studies.tail.map(renderStudy(patient))
  }

  def render = "tbody" #> SHtml.idMemoize { div =>
    LibraryPage.pageMemo(Full(div))
    LibraryPage.selectionList(Nil)

    "tr" #> Util.orderedGroupBy(page).map { case (patient, studies) =>
      renderPatient(patient, studies)
    }
  }
}

class LibraryRemove extends Loggable {
  def render = {
    LibraryPage.removeButtonId(nextFuncName)

    def remove() = {
      logger.info(LibraryPage.selectionList.get)
      val count = LibraryPage.selectionList.get.length
      LibraryPage.remove()
      S.notice(Text(s"Removed $count studies."))
      S.seeOther(S.uri)
    }
    def undo() = {
      val count = LibraryPage.undoList.get.length
      LibraryPage.undoRemove()
      S.notice(Text(s"Restored $count studies."))
      S.seeOther(S.uri)
    }

    "role=remove [id]" #> LibraryPage.removeButtonId.get &
    "role=remove [disabled]" #> "disabled" &
    "role=remove" #> SHtml.onSubmitUnit(remove) &
    "role=undo [disabled]" #> (if (LibraryPage.undoList.get.length == 0) Full("disabled") else Empty) &
    "role=undo" #> SHtml.onSubmitUnit(undo)
  }
}

class LibraryUpload {
  val user = ExtSessions.user.openOrThrowException("must be authenticated")

  def render = {
    var fileHolder: Box[FileParamHolder] = Empty

    def handleFile() = {
      fileHolder map { fph =>
        Await.result(Defer.uploadFuture(user, fph), duration.Duration.Inf)
      } map { file =>
        Defer.importFutureWithCallback(file) { result: ImportResultT =>
          code.comet.InboxServer ! (file, result)
        }
        S.notice("The file has been successfully uploaded")
      } openOr {
        S.warning("Please select a file to upload")
      }
      S.seeOther(Site.library.index.loc.calcDefaultHref)
    }

    "type=file" #> SHtml.fileUpload((fph) => fileHolder = Full(fph)) &
    "type=submit" #> SHtml.onSubmitUnit(handleFile)
  }
}
