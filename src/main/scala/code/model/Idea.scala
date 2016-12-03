package code.model

import code.lib._
import net.liftweb.http.S
import net.liftweb.util.Helpers
import org.joda.time.DateTime

import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.slick.driver.PostgresDriver.simple._
import code.lib.Slick._
import Database.dynamicSession

import scala.slick.model.ForeignKeyAction.{Cascade, Restrict}
import scala.xml.Text

case class IdeaId(value: Long) extends MappedTo[Long] with GenericId

object IdeaId {
  val empty = IdeaId(-1)
}

case class Idea(
  title: String
, description: String
, template: String = ""
, localId: String = ""
, domain: Option[String] = None
, ga: Option[String] = None
, createdAt: DateTime = DateTime.now
, updatedAt: DateTime = DateTime.now
, isPublished: Boolean = true
, isRemoved: Boolean = false
, userId: UserId = UserId.empty
, id: IdeaId = IdeaId.empty) {
  def touch: Idea = {
    val now = DateTime.now
    id match {
      case IdeaId.empty => copy(createdAt = now, updatedAt = now)
      case _ => copy(updatedAt = now)
    }
  }

  def matchesHost(host: String, tld: String): Boolean = {
    val isTld = domain.map(_.toLowerCase).contains(host.toLowerCase)
    val isSld = host.toLowerCase == s"$localId.$tld".toLowerCase
    isTld || isSld
  }

  def localUrl: String = s"${Site.scheme}://$localId.${Site.tld}${Site.port}"
  def externalUrlOpt: Option[String] = domain.map(d => s"${Site.scheme}://$d${Site.port}")
}

object Idea {
  final val empty = Idea("", "")

  object validate {
    val localIdRegex = """[a-zA-Z0-9_\\-]+""".r
    val domainRegex = """[a-zA-Z0-9_\\-\\.]*""".r

    val title = Validator.notEmpty[Idea]("title", Text("Please enter title"))(_.title)

    val description = Validator.notEmpty[Idea]("description", Text("Please enter description"))(_.description)

    val localId = Validator[Idea]("local-id", Text("This local domain is already used")) { idea: Idea =>
      !Ideas.isLocalIdUsed(idea.localId, idea.id)
    } & Validator.regex[Idea]("local-id", Text("Invalid characters"))(localIdRegex, _.localId)

    val domain = Validator[Idea]("domain", Text("This domain is already used")) { idea: Idea =>
      !Ideas.isDomainUsed(idea.domain, idea.id)
    } & Validator.regex[Idea]("domain", Text("Invalid characters"))(domainRegex, _.domain.getOrElse(""))

    val profile = title & description
    val settings = localId & domain
  }
}

class IdeasTable(tag: Tag) extends Table[Idea](tag, "ideas") {
  val title = column[String]("title")
  val description = column[String]("description")
  val template = column[String]("template")
  val localId = column[String]("local_id")
  val domain = column[Option[String]]("domain")
  val ga = column[Option[String]]("ga")
  val createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val updatedAt = column[DateTime]("updated_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val isPublished = column[Boolean]("is_published")
  val isRemoved = column[Boolean]("is_removed")
  val userId = column[UserId]("user_id")
  val id = column[IdeaId]("id", O.PrimaryKey, O.AutoInc)

  def localIdIndex = index("idx__ideas__local_id", localId, unique = true)
  def domainIndex = index("idx__ideas__domain", domain, unique = true)
  def userIdIndex = index("idx__ideas__user_id", userId)

  def user = foreignKey("fk__ideas__user_id", userId, Users)(_.id, onUpdate=Restrict, onDelete=Cascade)

  def * = (title, description, template, localId, domain, ga, createdAt, updatedAt, isPublished, isRemoved, userId, id) <>
    ((Idea.apply _).tupled, Idea.unapply)
}

object Ideas extends TableQuery(new IdeasTable(_)) with Loggable {
  val insertQuery = this returning this.map(_.id) into ((i, id) => i.copy(id = id))

  def insertOrUpdate(idea: Idea): Idea = {
    val newIdea = idea.touch
    newIdea.id match {
      case IdeaId.empty => insertQuery.insert(newIdea)
      case id =>
        byId(id).update(newIdea)
        newIdea
    }
  }

  val byId = for {
    id <- Parameters[IdeaId]
    i <- Ideas if i.id === id && !i.isRemoved
  } yield i

  def find(id: IdeaId): Box[Idea] = byId(id).firstOption
  def find(id: String): Box[Idea] = asLong(id).map(IdeaId.apply).flatMap(find)

  val byHost = for {
    host <- Parameters[String]
    i <- Ideas if (i.localId.toLowerCase === host.toLowerCase || i.domain.toLowerCase === host.toLowerCase) && !i.isRemoved
  } yield i

  def findByHost(host: String): Box[Idea] = byHost(host).firstOption

  def count: Long = Ideas.filter(!_.isRemoved).length.run

  def page(pageSize: Long, curPage: Long, sort: IdeasTable => Column[_], asc: Boolean): Seq[Idea] = {
    Ideas.filter(!_.isRemoved)
      .sortBy(t => if (asc) sort(t).asc else sort(t).desc)
      .drop(pageSize * curPage).take(pageSize).list
  }

  def countForUser(userId: UserId): Long = Ideas.filter(u => u.userId === userId && !u.isRemoved).length.run

  def pageForUser(userId: UserId, pageSize: Long, curPage: Long, sort: IdeasTable => Column[_], asc: Boolean): Seq[Idea] = {
    Ideas.filter(i => i.userId === userId && !i.isRemoved)
         .sortBy(t => if (asc) sort(t).asc else sort(t).desc)
         .drop(pageSize * curPage).take(pageSize).list
  }

  def nextLocalId(title: String): String = {
    val base: String = Option(title.toLowerCase.replaceAll("[^a-zA-Z0-9_\\-]", ""))
                             .filter(_.length > 0).getOrElse("idea")

    val idStream = Stream.from(0).map {
      case 0 => base
      case x => s"$base-$x"
    }

    idStream.find(id => Ideas.filter(i => i.localId === id && !i.isRemoved).length.run == 0).getOrElse(Helpers.randomString(10))
  }

  def isLocalIdUsed(id: String, ideaId: IdeaId): Boolean = {
    Ideas.filter(i => i.localId === id && i.id =!= ideaId && !i.isRemoved).length.run > 0
  }

  def isDomainUsed(domain: Option[String], ideaId: IdeaId): Boolean = {
    Ideas.filter(i => i.domain === domain && i.id =!= ideaId && !i.isRemoved).length.run > 0
  }

  def remove(idea: Idea) = {
    val newLocalId = nextLocalId(idea.localId + "-removed")
    Ideas.filter(_.id === idea.id).update(idea.copy(isRemoved = true, localId = newLocalId, domain = None))
  }

  val ideasWithUser = Compiled { (d: ConstColumn[Long], t: ConstColumn[Long]) =>
    Ideas.join(Users).on((i, u) => i.userId === u.id).drop(d).take(t)
  }

  val ideasWithUserCount = Compiled { Ideas.join(Users).on((i, u) => i.userId === u.id).length }

  def findIdeasWithUser(drop: Long, take: Long): Seq[(Idea, User)] = ideasWithUser(drop, take).run
  def countIdeasWithUser: Long = ideasWithUserCount.run
}
