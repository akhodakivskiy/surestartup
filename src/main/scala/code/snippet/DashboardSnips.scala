package code.snippet

import code.lib._
import code.model._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.util._
import net.liftweb.util.Helpers._

import scala.xml.Text

class Dashboard(idea: Idea) extends Loggable {
  def published = {
    ";offline" #> (if (idea.isPublished) ClearNodes else PassThru) &
    ";online" #> (if (idea.isPublished) PassThru else ClearNodes)
  }

  def edit = {
    "^ [onclick]" #> SHtml.ajaxInvoke { () =>
      S.redirectTo(Site.edit.calcHref((idea, "index")), () => CurrentIdea.load(idea))
    }
  }

  def signups = "^ [href]" #> Site.signups.calcHref(idea)

  def remove = {
    val confirmBoxId = nextFuncName
    var localId = ""

    def process(): Unit = {

      logger.info(localId)

      if (localId == idea.localId) {
        Ideas.remove(idea)
        S.notice(<span>Successfully removed page <span class="text-bold">{idea.localId}</span></span>)
        S.redirectTo(Site.pages.loc.calcDefaultHref)
      } else {
        S.error("remove-local-id", "Local ID doesn't match. Please try again.")
        S.redirectTo(S.uri)
      }
    }

    ";confirm-box [id]" #> confirmBoxId &
    ";confirm-box [style]" #> "display:none" &
    ";remove [onclick]" #> JsCmds.JsShowId(confirmBoxId) &
    ";remove-local-id" #> SHtml.text(localId, localId = _) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }

  def controls = {
    var i: Idea = idea

    ";status-checkbox" #> SHtml.checkbox(idea.isPublished, v => i = i.copy(isPublished = v)) &
    ";local-id" #> SHtml.text(idea.localId, v => i = i.copy(localId = v.trim)) &
    ";domain" #> SHtml.text(idea.domain.getOrElse(""), v => i = i.copy(domain = Option(v.trim).filter(!_.isEmpty))) &
    ";sld *" #> s"${idea.localId}.${Site.tld}" &
    ";ga" #> SHtml.text(idea.ga.getOrElse(""), v => i = i.copy(ga = Option(v.trim).filter(!_.isEmpty))) &
    ";dns-help" #> idea.domain.map(dnsHelp).getOrElse(ClearNodes) &
    ";links" #> (if (idea.isPublished) links else ClearNodes) &
    "type=submit" #> SHtml.onSubmitUnit { () =>
      Idea.validate.settings.run(i) match {
        case Nil => i = Ideas.insertOrUpdate(i)
        case errors => errors.flash()
      }
      S.redirectTo(Site.dashboard.calcHref(i))
    }
  }

  def dnsHelp(domain: String) = {
    val dnsMsgId = nextFuncName
    def dnsLookup(): JsCmd = {
      val cname: Box[String] = MyHelpers.dnsLookup(domain, "CNAME")
      if (cname.contains(s"${Site.tld}.")) {
        JsCmds.SetHtml(dnsMsgId, <span class="text-success">Success!</span>)
      } else {
        val currentValue = <span class="text-bold">{cname.getOrElse("none")}</span>
        JsCmds.SetHtml(dnsMsgId, <span class="text-danger">Failure. Current CNAME record vlaue: {currentValue}</span>)
      }
    }

    ";dns-lookup [onclick]" #> SHtml.ajaxInvoke(dnsLookup) &
    ";dns-msg [id]" #> dnsMsgId
  }

  def links = {
    ";host *" #> Site.tld &
    ";link-local [href]" #> idea.localUrl &
    ";link-local *" #> idea.localUrl &
    ";item-domain" #> {
      idea.externalUrlOpt match {
        case Some(externalUrl) =>
          ";link-domain [href]" #> externalUrl &
          ";link-domain *" #> externalUrl
        case None => ClearNodes
      }
    }
  }
}
