package code.lib

import code.model._
import code.snippet._

import net.liftweb._
import net.liftweb.util.Helpers.{asLong}
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._

import scala.xml.NodeSeq

object Site extends Loggable {
  val NavBarGroup = LocGroup("navbar")

  val RequireLoggedIn: AnyLocParam = If(
    () => ExtSessions.isLoggedIn,
    () => RedirectResponse(Site.login.loc.calcDefaultHref))

  val RequireNotLoggedIn: AnyLocParam = If(
    () => !ExtSessions.isLoggedIn,
    () => RedirectResponse(Site.index.loc.calcDefaultHref))

  val SignOutRedirect: AnyLocParam = Loc.EarlyResponse(() => {
    ExtSessions.logOut
    Full(RedirectResponse(Site.login.loc.calcDefaultHref, S.responseCookies:_*))
  })
  case class Extractor[ID, T](toId: (Long) => ID, toBoxT: (ID) => Box[T]) {
    def apply(id: String): Box[T] = asLong(id).map(toId).flatMap(apply)
    @inline def apply(id: ID): Box[T] = toBoxT(id)
  }
  def TestUserAccess[T](test: (T, UserT) => Boolean) = Loc.TestValueAccess { box: Box[T] =>
    for {
      value <- box
      user <- ExtSessions.user if !test(value, user)
    } yield NotFoundResponse(s"Resource does not exist")
  }

  val WithSearch: AnyLocParam = Snippet("IgnoreSearch", PassThru)

  val index = Menu.i("Home") / "index"
  val login = Menu.i("Login") / "login" >> RequireNotLoggedIn
  val signup = Menu.i("Sign Up") / "signup" >> RequireNotLoggedIn
  val signout = Menu.i("Sign Out") / "signout" >> RequireLoggedIn >> SignOutRedirect
  val forgotPassword = Menu.i("Forgot Password") / "forgot-password" >> RequireNotLoggedIn
  val profile = Menu.i("Profile") / "profile" >> RequireLoggedIn

  val activate = Menu.param[UserTokenT](
    "Activate",
    "Activate",
    UserTokens.find(_, TokenType.Activate),
    _.token
  ) / "activate" >> RequireNotLoggedIn

  val resetPassword = Menu.param[UserTokenT](
    "Reset Password",
    "Reset Password",
    UserTokens.find(_, TokenType.ResetPassword),
    _.token
  ) / "reset-password" >> RequireNotLoggedIn

  /****** Library ******/

  object library {
    class DicomVar[T, ID](
      toId: (Long) => ID
    , toBoxT: (ID) => Box[T]
    , propagate: (T) => Any
    ) extends RequestVar[Box[T]](Empty) {
      val extractor = Extractor(toId, toBoxT)
      def apply(id: String): Box[T] = extractor(id).flatMap(update)
      def apply(id: ID): Box[T] = extractor(id).flatMap(update)
      def update(t: T): Box[T] = {
        propagate(t)
        apply(Full(t))
      }
    }

    object PatientVar extends DicomVar(PatientId(_), Patients.find, (t: PatientT) => Unit)
    object StudyVar extends DicomVar(StudyId(_), Studies.find, (s: StudyT) => PatientVar(s.patientId))
    object SeriesVar extends DicomVar(SeriesId(_), Seriess.find, (s: SeriesT) => StudyVar(s.studyId))
    object ImageVar extends DicomVar(ImageId(_), Images.find, (i: ImageT) => SeriesVar(i.seriesId))

    val TestPatientAccess = TestUserAccess[PatientT](_.userId == _.id)
    val TestStudyAccess = TestUserAccess(Studies.canAccess)
    val TestSeriesAccess = TestUserAccess(Seriess.canAccess)
    val TestImageAccess = TestUserAccess(Images.canAccess)
    val TestThumbnailAccess = TestUserAccess(Thumbnails.canAccess)

    val thumbnail: Menu.ParamMenuable[ThumbnailT] = (Menu.param[ThumbnailT](
      "Thumbnail", "Thumbnail",
      Extractor(ThumbnailId(_), Thumbnails.find).apply,
      _.id.toString
    ) / "library" / "image" / "thumbnail" / *
      >> TestThumbnailAccess
      >> EarlyResponse(() => {
      thumbnail.currentValue.map { thumbnail =>
        OutputStreamResponse { os: java.io.OutputStream =>
          val path = java.nio.file.Paths.get(Storage.inst.url(thumbnail.key))
          java.nio.file.Files.copy(path, os)
        }
      }
    }))

    val image = (Menu.param[ImageT](
      "Image", "Image",
      ImageVar.apply,
      _.id.toString
    ) / "library" / "image"
      >> TestImageAccess
      >> CalcValue(() => ImageVar.get)
      submenus(thumbnail))

    val study = (Menu.param[StudyT](
      "Study", "Study",
      StudyVar.apply,
      _.id.toString
    ) / "library" / "study"
      >> TestStudyAccess
      >> CalcValue(() => StudyVar.get)
      submenus(image))

    val patient = (Menu.param[PatientT](
      "Patient", "Patient",
      PatientVar.apply,
      _.id.toString
    ) / "library" / "patient"
      >> TestPatientAccess
      >> CalcValue(() => PatientVar.get)
      submenus(study))

    val index = (Menu.i("Library") / "library"
      >> WithSearch
      >> RequireLoggedIn
      >> NavBarGroup
      submenus(patient))
  }

  /****** Files ******/

  object files {
    val TestFileAccess = TestUserAccess[FileT](_.userId == _.id)

    val download: Menu.ParamMenuable[FileT] = (Menu.param[FileT](
      "File", "File",
      asLong(_).map(FileId(_)).flatMap(Files.find),
      _.id.toString
    ) / "files" / "download"
      >> TestFileAccess
      >> EarlyResponse(() => {
      download.currentValue match {
        case Full(file) => {
          val writer: (java.io.OutputStream) => Unit = { os =>
            val path = java.nio.file.Paths.get(Storage.inst.url(file.key))
            java.nio.file.Files.copy(path, os)
          }
          Full(OutputStreamResponse(writer))
        }
        case _ => Full(NotFoundResponse("file not found"))
      }
    }))

    val tags = Menu.param[FileT](
      "Tags", "DICOM Tags",
      asLong(_).map(FileId(_)).flatMap(Files.findArchive),
      _.id.toString
    ) / "files" / "tags" >> TestFileAccess

    val archive = Menu.param[FileT](
      "Archive", "Archive",
      asLong(_).map(FileId(_)).flatMap(Files.findArchive),
      _.id.toString
    ) / "files" / "archive" >> TestFileAccess

    val index = (Menu.i("Files") / "files"
      >> RequireLoggedIn
      >> NavBarGroup
      submenus(download, tags, archive))
  }

  val menus =
    List(
      index
    , login
    , signup
    , signout
    , activate
    , forgotPassword
    , resetPassword
    , profile
    , library.index
    , files.index)

  val siteMap = SiteMap(menus: _*)
}
