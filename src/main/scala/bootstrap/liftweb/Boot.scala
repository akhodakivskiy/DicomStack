package bootstrap.liftweb

import net.liftweb._
import net.liftweb.util._
import net.liftweb.util.Helpers._

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.http.js.jquery._

import code.lib._
import code.model._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Loggable {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("code")

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Upload file size capped at 100Mb
    LiftRules.maxMimeSize = 100 * 1024 * 1024
    LiftRules.maxMimeFileSize = 100 * 1024 * 1024

    // Store uploaded files on disk
    LiftRules.handleMimeFile = OnDiskFileParamHolder.apply

    // dcm4che setup
    Dicom.boot

    // Set up extended session wrappers
    ExtSessions.boot

    // Init the database
    Slick.boot

    // Setup Mailer
    Mail.boot

    // And finally setup the sitemap!
    LiftRules.setSiteMap(Site.siteMap)
  }
}
