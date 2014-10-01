package code.snippet

import code.model._
import code.lib._

import java.io.InputStream

import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import Slick._

class PatientSnips(patient: PatientT) extends Loggable {
  def name = "* *" #> patient.fullName

  def details = {
    "role=fullName *" #> patient.fullName &
    "role=sex *" #> patient.sex.toString &
    "role=dateOfBirth *" #> Util.DTF.localShort.print(patient.birthDate) &
    "role=id *" #>  patient.identifier &
    "role=idIssuer *" #>  patient.identifierIssuer &
    "role=idOther *" #> patient.otherIdentifiers &
    "role=address *" #>  patient.address &
    "role=phone *" #> patient.phone
  }
}

class PatientStudiesPage(patient: PatientT) extends EmptyableSnippet with Loggable {
  val studies = Studies.findByPatientId(patient.id)

  override def isEmpty = studies.size == 0

  def render = {
    "tr *" #> studies.zipWithIndex.map { case (study, index) =>
      "role=index *" #> (index + 1) &
      "role=description *" #> {
        "a *" #> Util.maybeUnnamed(study.description, Util.unnamedStudy) &
        "a [href]" #> Site.library.study.calcHref(study)
      } &
      "role=modality *" #> study.modality &
      "role=date *" #> Util.DTF.localShort.print(study.date)
    }
  }
}
