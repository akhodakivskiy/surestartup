package code.snippet

import code.lib._
import code.model._
import net.liftweb.builtin.snippet.Msg
import net.liftweb.common._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.{JE, JsCmds}
import net.liftweb.http.js.jquery.JqJE._
import net.liftweb.http.{RequestVar, LiftSession, S, SHtml}
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.{PassThru, ClearNodes}
import net.liftweb.util.Helpers._

import scala.xml.{NodeSeq, Text}

class ControlsCheck {
  def render = {
    S.location.exists(_.params.contains(Site.RequireDomain)) match {
      case true => ClearNodes
      case false => PassThru
    }
  }
}

trait IdeaControls {
  def cancelLink: String
  def prevLinkBox: Box[String]
  def nextLinkBox: Box[String]
  def currentPage: Int
  def totalPages: Int
  def isAuthorized: Boolean

  val currentUri = S.uriAndQueryString.getOrElse("/")
  val signupUri = Site.signup.loc.calcDefaultHref
  val loginUri = Site.login.loc.calcDefaultHref

  def render = {
    def when(cond: => Boolean)(snippet: NodeSeq => NodeSeq): NodeSeq => NodeSeq = {
      if (cond) snippet else ClearNodes
    }

    ";cancel" #> ("a [href]" #> cancelLink) &
    ";prev" #> prevLinkBox.map(uri => "a [onclick]" #> SHtml.ajaxInvoke { () =>
      S.redirectTo(uri, CurrentIdea.snapshot())
    }) &
    ";next" #> nextLinkBox.map(uri => "a [onclick]" #> SHtml.ajaxInvoke { () =>
      S.redirectTo(uri, CurrentIdea.snapshot())
    }) &
    ";page" #> when(totalPages > 1) {
      ";current *" #> currentPage &
      ";total *" #> totalPages
    } &
    ";save" #> when(isAuthorized) {
      "a [onclick]" #> SHtml.ajaxInvoke { () =>
        CurrentIdea.persist()
        S.redirectTo(Site.dashboard.calcHref(CurrentIdea.idea))
      }
    } &
    ";auth" #> when(!isAuthorized) {
      "a [onclick]" #> SHtml.ajaxInvoke { () =>
        S.redirectTo(Site.login.loc.calcDefaultHref, () => {
          S.notice(
            <span>
              Please <a href={loginUri}>log in</a> or <a href={signupUri}>sign up</a> to save and publish the page.
              Your work is saved in the current session and won't disappear until you authenticate.
              Click <a href={currentUri}>here</a> to return to the page editor.
            </span>
          )
        })
      }
    }
  }
}

class TryCreateControls(template: IdeaTemplate, page: String) extends IdeaControls {
  def cancelLink: String = S.mapFuncToURI(Site.start.loc.calcDefaultHref, () => TryCreateIdea.apply(Empty))
  def prevLinkBox: Box[String] = template.prevPage(page).map(Site.tryCreate.calcHref)
  def nextLinkBox: Box[String] = template.nextPage(page).map(Site.tryCreate.calcHref)
  def currentPage = template.pages.indexOf(page) + 1
  def totalPages = template.pages.length
  def isAuthorized = false
}

class CreateControls(template: IdeaTemplate, page: String) extends IdeaControls {
  def cancelLink: String = Site.newPage.loc.calcDefaultHref
  def prevLinkBox: Box[String] = template.prevPage(page).map(p => Site.create.calcHref((template, p)))
  def nextLinkBox: Box[String] = template.nextPage(page).map(p => Site.create.calcHref((template, p)))
  def currentPage = template.pages.indexOf(page) + 1
  def totalPages = template.pages.length
  def isAuthorized = true
}

class EditControls(idea: Idea, page: String) extends IdeaControls {
  val template = IdeaTemplate.find(idea.template)

  def cancelLink: String = Site.dashboard.calcHref(idea)
  def prevLinkBox: Box[String] = template.flatMap(_.prevPage(page)).map(p => Site.edit.calcHref((idea, p)))
  def nextLinkBox: Box[String] = template.flatMap(_.nextPage(page)).map(p => Site.edit.calcHref((idea, p)))
  def currentPage = template.map(_.pages.indexOf(page) + 1).getOrElse(-1)
  def totalPages = template.map(_.pages.length).getOrElse(0)
  def isAuthorized = true
}
