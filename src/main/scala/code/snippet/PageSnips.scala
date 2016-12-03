package code.snippet

import code.lib._
import code.model._
import net.liftweb.common.{Full, Loggable}
import net.liftweb.http.js.JsCmds
import net.liftweb.http.{S, SHtml}
import net.liftweb.util._
import net.liftweb.util.Helpers._
import org.joda.time.format.DateTimeFormat

import scala.slick.lifted.Column

class IdeasPage extends MySortedPaginatorSnippet[Idea, (IdeasTable) => Column[_]] with Loggable {
  val userIdBox = CurrentUser.get.map(_.user.id)

  override val itemsPerPage: Int = 10

  val headers = List[(String, IdeasTable => Column[_])](
    ("title", _.title),
    ("localid", _.localId),
    ("updated", _.updatedAt),
    ("published", _.isPublished))

  val count: Long = userIdBox.map(id => Ideas.countForUser(id)).getOrElse(0)

  val page: Seq[Idea] = userIdBox match {
    case Full(userId) => Ideas.pageForUser(userId, itemsPerPage, curPage, headers(sort._1)._2, sort._2)
    case _ => Seq.empty
  }

  val signupCounts: Map[IdeaId, Int] = IdeaSignups.count(page.map(_.id).toSet)

  val formatter = DateTimeFormat.forPattern("yyyy/MM/dd hh:mm:ss")

  def render = {
    def conditional(cond: Boolean)(bind: CssSel): CssSel = if (cond) bind else "^" #> ClearNodes

    "tr" #> page.map { idea =>
      ";title *" #> idea.title &
      ";title [href]" #> Site.dashboard.calcHref(idea) &
      ";local-id *" #> idea.localId &
      ";updated-at *" #> formatter.print(idea.updatedAt) &
      ";local-link" #> conditional(idea.isPublished)("^ [href]" #> idea.localUrl) &
      ";external-link" #> conditional(idea.isPublished && idea.domain.isDefined)("^ [href]" #> idea.externalUrlOpt) &
      ";signups" #> conditional(signupCounts.get(idea.id).exists(_ > 0)) {
        "^ [href]" #> Site.signups.calcHref(idea) &
        "^ *" #> signupCounts.get(idea.id)
      }
    }
  }

  def isEmpty = if (count > 0) ClearNodes else PassThru
  def isNotEmpty = if (count == 0) ClearNodes else PassThru
}

class SignupsPage(idea: Idea) extends MySortedPaginatorSnippet[IdeaSignup, (IdeaSignupsTable) => Column[_]] with Loggable {
  override val itemsPerPage: Int = 10

  val headers = List[(String, IdeaSignupsTable => Column[_])](
    ("email", _.email),
    ("name", _.name),
    ("message", _.message),
    ("date", _.createdAt))

  val count: Long = IdeaSignups.countForIdea(idea.id)

  val page: Seq[IdeaSignup] = {
    IdeaSignups.pageForIdea(idea.id, itemsPerPage, curPage, headers(sort._1)._2, sort._2)
  }

  val formatter = DateTimeFormat.forPattern("yyyy/MM/dd hh:mm:ss")

  def render = {
    "tr" #> page.map { is =>
      ";email *" #> is.email &
      ";name *" #> is.name &
      ";message *" #> is.message &
      ";date *" #> formatter.print(is.createdAt)
    }
  }

  def csv = {
    "^ [href]" #> Site.signupsCsv.calcHref(idea)
  }

  def isEmpty = if (count > 0) ClearNodes else PassThru
  def isNotEmpty = if (count == 0) ClearNodes else PassThru
}
