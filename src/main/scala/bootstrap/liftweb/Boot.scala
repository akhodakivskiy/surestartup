package bootstrap.liftweb

import code.lib._
import net.liftweb.common.Loggable
import net.liftweb.http._

/**
 * Created by anton on 9/26/14.
 */
class Boot extends Loggable {
  def boot = {
    LiftRules.addToPackages("code")

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))

    // Store uploaded files on disk
    LiftRules.handleMimeFile = OnDiskFileParamHolder.apply


    ImageUpload.boot()

    Mail.boot()

    Site.boot()

    Slick.boot()
  }
}
