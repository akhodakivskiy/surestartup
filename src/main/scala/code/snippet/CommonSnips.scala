package code.snippet

import java.util.{GregorianCalendar, Date}

import code.lib.Site
import code.model.Idea
import net.liftweb.common.Full
import net.liftweb.http.S

import net.liftweb.util.Helpers._
import net.liftweb.util.{Props, ClearNodes, PassThru}
import org.joda.time.DateTime

class CNAMEHost {
  def render = "^ *" #> Site.tld
}

class ProjectName {
  def render = "^ *" #> Site.name
}

class IdeaSnips(idea: Idea) {
  def title = "^ *" #> idea.title
}

class ProjectGA {
  def render = Props.getBool("project.ga", defVal = false) match {
    case true => PassThru
    case false => ClearNodes
  }
}

class CurrentYear {
  def render = "^ *" #> new DateTime().year().getAsString
}

class BreadCrumbs {
  def render = {
    S.location.map(_.breadCrumbs) match {
      case Full(items) if items.length < 2 => "^" #> ClearNodes
      case Full(items) => "li" #> items.map {
        case loc if S.location.exists(_.name == loc.name) =>
          "^ *" #> loc.title
        case loc =>
          "a *" #> loc.title & "a [href]" #> loc.calcDefaultHref
      }
      case _ => "^" #> ClearNodes
    }
  }
}
