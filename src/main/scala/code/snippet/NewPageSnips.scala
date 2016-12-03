package code.snippet

import code.model.Idea
import net.liftweb.common.Full
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.ClearNodes
import net.liftweb.util.Helpers._

import code.lib._

class ResumeLastTemplate {
  def render = TryCreateIdea.get match {
    case Full(_) => "^ [href]" #> Site.tryCreate.calcHref("index")
    case _ => ClearNodes
  }
}

class ChooseTemplate {
  def continue = "^ [href]" #> Site.tryCreate.calcHref("index")

  def render = {
    "^" #> IdeaTemplate.all.map { template =>
      ";start [onclick]" #> SHtml.ajaxInvoke { () =>
        CurrentUser.get match {
          case Full(UserWithAuthInfo(user, _)) =>
            S.redirectTo(Site.create.calcHref((template, "index")), () => {
              CurrentIdea.load(Idea.empty.copy(template = template.name, userId = user.id))
            })
          case _ =>
            TryCreateIdea.load(Idea.empty.copy(template = template.name))
            S.redirectTo(Site.tryCreate.calcHref("index"))
        }
      } &
        ";screenshot [src]" #> template.screenshotPath &
        ";description *" #> template.description &
        ";title *" #> template.title
    }
  }
}
