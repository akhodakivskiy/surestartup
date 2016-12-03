package code.model

import org.joda.time.DateTime

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import ForeignKeyAction.{Cascade, Restrict}
import code.lib.Slick._


case class IdeaTextId(value: Long) extends MappedTo[Long] with GenericId

object IdeaTextId {
  val empty = IdeaTextId(-1)
}

case class IdeaText(
  name: String
, text: String
, ideaId: IdeaId = IdeaId.empty
, createdAt: DateTime = DateTime.now
, updatedAt: DateTime = DateTime.now
, isRemoved: Boolean = false
, id: IdeaTextId = IdeaTextId.empty) {
  def touch: IdeaText = {
    val now = DateTime.now
    id match {
      case IdeaTextId.empty => copy(createdAt = now, updatedAt = now)
      case _ => copy(updatedAt = now)
    }
  }
}

class IdeaTextsTable(tag: Tag) extends Table[IdeaText](tag, "idea_texts") {
  val name = column[String]("name")
  val text = column[String]("text")
  val ideaId = column[IdeaId]("idea_id")
  val createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val updatedAt = column[DateTime]("updated_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val isRemoved = column[Boolean]("is_removed")
  val id = column[IdeaTextId]("id", O.PrimaryKey, O.AutoInc)

  def ideaIdIndex = index("idx__idea_texts__idea_id", ideaId)

  def idea = foreignKey("fk__idea_texts__idea_id", ideaId, Ideas)(_.id, onUpdate=Restrict, onDelete=Cascade)

  def * = (name, text, ideaId, createdAt, updatedAt, isRemoved, id) <>
    ((IdeaText.apply _).tupled, IdeaText.unapply)
}

object IdeaTexts extends TableQuery(new IdeaTextsTable(_)) {
  val insertQuery = this returning this.map(_.id) into ((t, id) => t.copy(id = id))

  def insertOrUpdate(text: IdeaText): IdeaText = {
    val newText = text.touch
    newText .id match {
      case IdeaTextId.empty => insertQuery.insert(newText)
      case id =>
        byId(id).update(newText)
        newText
    }
  }

  val byId = for {
    id <- Parameters[IdeaTextId]
    b <- IdeaTexts if b.id === id && !b.isRemoved
  } yield b

  def find(id: IdeaTextId): Option[IdeaText] = byId(id).firstOption

  val byIdeaId = for {
    ideaId <- Parameters[IdeaId]
    b <- IdeaTexts if b.ideaId === ideaId && !b.isRemoved
  } yield b

  def find(id: IdeaId): List[IdeaText] = byIdeaId(id).list
}
