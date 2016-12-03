package code.snippet

import code.lib.{MySortedPaginatorSnippet, Site, CurrentUser}
import code.model._
import net.liftweb.http.PaginatorSnippet
import net.liftweb.util._
import net.liftweb.util.Helpers._
import org.joda.time.format.DateTimeFormat

import scala.slick.lifted.Column

class AdminUsersPage extends MySortedPaginatorSnippet[User, UsersTable => Column[_]] {
  override def itemsPerPage = 10

  val headers = List[(String, UsersTable => Column[_])](
    ("id", _.id),
    ("first", _.first),
    ("last", _.last),
    ("email", _.email),
    ("updated", _.updatedAt),
    ("active", _.isActive))

  override val count: Long = Users.count

  override val page: Seq[User] = {
    Users.page(itemsPerPage, curPage, headers(sort._1)._2, sort._2)
  }

  def render = {
    val formatter = DateTimeFormat.forPattern("yyyy/MM/dd hh:mm")
    val authInfos = UserAuthInfos.findByUserIds(page.map(_.id))

    "^" #> page.map { user =>
      val providerBox = authInfos.find(_.userId == user.id).map(_.provider) map {
        case AuthInfoProvider.Facebook => <i class="fa fa-facebook"></i>
        case AuthInfoProvider.Twitter => <i class="fa fa-twitter"></i>
        case AuthInfoProvider.Unknown => <i class="fa fa-question"></i>
      }
      ";id *" #> user.id.toString &
      ";first *" #> user.first &
      ";last *" #> user.last &
      ";email *" #> user.email &
      ";updated-at *" #> formatter.print(user.updatedAt) &
      ";active *" #> (if (user.isActive) PassThru else ClearNodes) &
      ";auth *" #> providerBox
    }
  }

  def isEmpty = if (count > 0) ClearNodes else PassThru
  def isNotEmpty = if (count == 0) ClearNodes else PassThru
}

class AdminIdeasPage extends MySortedPaginatorSnippet[Idea, IdeasTable => Column[_]] {
  override def itemsPerPage = 10

  val headers = List[(String, IdeasTable => Column[_])](
    ("id", _.id),
    ("userid", _.userId),
    ("title", _.title),
    ("localid", _.localId),
    ("updated", _.updatedAt),
    ("published", _.isPublished))

  val count: Long = Ideas.count

  val page: Seq[Idea] = {
    Ideas.page(itemsPerPage, curPage, headers(sort._1)._2, sort._2)
  }

  val formatter = DateTimeFormat.forPattern("yyyy/MM/dd hh:mm")

  def render = {
    "^" #> page.map { case idea =>
      ";id *" #> idea.id.toString &
      ";user-id *" #> idea.userId.toString &
      ";published *" #> (if (idea.isPublished) PassThru else ClearNodes) &
      ";title *" #> idea.title &
      ";title [href]" #> Site.dashboard.calcHref(idea) &
      ";local-id *" #> idea.localId &
      ";updated-at *" #> formatter.print(idea.updatedAt)
    }
  }

  def isEmpty = if (count > 0) ClearNodes else PassThru
  def isNotEmpty = if (count == 0) ClearNodes else PassThru
}
