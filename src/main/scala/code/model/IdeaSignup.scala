package code.model

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import ForeignKeyAction.{Cascade, Restrict}
import code.lib.Slick._

case class IdeaSignupId(value: Long) extends MappedTo[Long] with GenericId

object IdeaSignupId{
  val empty = IdeaSignupId(-1)
}

case class IdeaSignup(
  email: String
, name: String
, message: String
, ideaId: IdeaId = IdeaId.empty
, createdAt: DateTime = DateTime.now
, updatedAt: DateTime = DateTime.now
, isRemoved: Boolean = false
, id: IdeaSignupId = IdeaSignupId.empty) {
  def touch: IdeaSignup = {
    val now = DateTime.now
    id match {
      case IdeaSignupId.empty => copy(createdAt = now, updatedAt = now)
      case _ => copy(updatedAt = now)
    }
  }
}

object IdeaSignup {
  implicit class IdeaSignupListToCSV(signups: List[IdeaSignup]) {
    def csv(): List[String] = {
      val formatter = DateTimeFormat.forPattern("yyyy/MM/dd hh:mm:ss")
      val header = "email,name,message,date"
      val rows = signups.map { is =>
        s""""${is.email}","${is.name}","${is.message}","${formatter.print(is.createdAt)}""""
      }
      header :: rows
    }
  }
}

class IdeaSignupsTable(tag: Tag) extends Table[IdeaSignup](tag, "idea_signups") {
  val email = column[String]("email")
  val name = column[String]("name")
  val message = column[String]("message", O.DBType("TEXT"))
  val ideaId = column[IdeaId]("idea_id")
  val createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val updatedAt = column[DateTime]("updated_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val isRemoved = column[Boolean]("is_removed")
  val id = column[IdeaSignupId]("id", O.PrimaryKey, O.AutoInc)

  def ideaIdIndex = index("idx__idea_signups__idea_id", ideaId)

  def idea = foreignKey("fk__idea_signups__idea_id", ideaId, Ideas)(_.id, onUpdate=Restrict, onDelete=Cascade)

  def * = (email, name, message, ideaId, createdAt, updatedAt, isRemoved, id) <>
    ((IdeaSignup.apply _).tupled, IdeaSignup.unapply)
}

object IdeaSignups extends TableQuery(new IdeaSignupsTable(_)) {
  val insertQuery = this returning this.map(_.id) into ((s, id) => s.copy(id = id))

  def insertOrUpdate(signup: IdeaSignup): IdeaSignup = {
    val newSignup = signup.touch
    newSignup.id match {
      case IdeaSignupId.empty => insertQuery.insert(newSignup)
      case id =>
        byId(id).update(newSignup)
        newSignup
    }
  }

  val byId = for {
    id <- Parameters[IdeaSignupId]
    b <- IdeaSignups if b.id === id && !b.isRemoved
  } yield b

  def find(id: IdeaSignupId): Option[IdeaSignup] = byId(id).firstOption

  val byIdeaId = for {
    ideaId <- Parameters[IdeaId]
    b <- IdeaSignups if b.ideaId === ideaId && !b.isRemoved
  } yield b

  def find(id: IdeaId): List[IdeaSignup] = byIdeaId(id).list

  def count(ids: Set[IdeaId]): Map[IdeaId, Int] = {
    this.filter(_.ideaId inSet ids).groupBy(_.ideaId).map {
      case (id, results) => id -> results.length
    }.run.toMap
  }

  def countForIdea(ideaId: IdeaId): Long = IdeaSignups.filter(s => s.ideaId === ideaId && !s.isRemoved).length.run

  def pageForIdea(ideaId: IdeaId, pageSize: Long, curPage: Long, sort: IdeaSignupsTable => Column[_], asc: Boolean): Seq[IdeaSignup] = {
    IdeaSignups.filter(s => s.ideaId === ideaId && !s.isRemoved)
      .sortBy(t => if (asc) sort(t).asc else sort(t).desc)
      .drop(pageSize * curPage).take(pageSize).list
  }
}
