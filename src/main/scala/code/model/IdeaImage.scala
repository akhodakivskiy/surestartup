package code.model

import java.io.{InputStream, File}
import java.nio.file.{Files, Path, Paths}
import javax.imageio.ImageIO

import net.liftweb.common.{Loggable, Empty, Full, Box}
import net.liftweb.http.{S, LiftSession}
import net.liftweb.util.Props
import org.imgscalr.Scalr
import org.joda.time.DateTime

import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession
import ForeignKeyAction.{Cascade, Restrict}
import code.lib.Slick._

import scala.util.Try


case class IdeaImageId(value: Long) extends MappedTo[Long] with GenericId

object IdeaImageId {
  val empty = IdeaImageId(-1)
}

case class IdeaImage(
  name: String
, path: String
, height: Int
, width: Int
, ideaId: IdeaId = IdeaId.empty
, createdAt: DateTime = DateTime.now
, updatedAt: DateTime = DateTime.now
, isRemoved: Boolean = false
, id: IdeaImageId = IdeaImageId.empty) {
  def touch: IdeaImage = {
    val now = DateTime.now
    id match {
      case IdeaImageId.empty => copy(createdAt = now, updatedAt = now)
      case _ => copy(updatedAt = now)
    }
  }
}

object IdeaImage extends Loggable {
}

class IdeaImagesTable(tag: Tag) extends Table[IdeaImage](tag, "idea_images") {
  val name = column[String]("name")
  val path = column[String]("path")
  val height = column[Int]("height")
  val width = column[Int]("width")
  val ideaId = column[IdeaId]("idea_id")
  val createdAt = column[DateTime]("created_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val updatedAt = column[DateTime]("updated_at", O.DBType("TIMESTAMP WITH TIME ZONE"))
  val isRemoved = column[Boolean]("is_removed")
  val id = column[IdeaImageId]("id", O.PrimaryKey, O.AutoInc)

  def ideaIdIndex = index("idx__idea_images__idea_id", ideaId)

  def idea = foreignKey("fk__idea_images__idea_id", ideaId, Ideas)(_.id, onUpdate=Restrict, onDelete=Cascade)

  def * = (name, path, height, width, ideaId, createdAt, updatedAt, isRemoved, id) <>
    ((IdeaImage.apply _).tupled, IdeaImage.unapply)
}

object IdeaImages extends TableQuery(new IdeaImagesTable(_)) {
  val insertQuery = this returning this.map(_.id) into ((i, id) => i.copy(id = id))

  def insertOrUpdate(image: IdeaImage): IdeaImage = {
    val newImage = image.touch
    newImage.id match {
      case IdeaImageId.empty => insertQuery.insert(newImage)
      case id =>
        byId(id).update(newImage)
        newImage
    }
  }

  val byId = for {
    id <- Parameters[IdeaImageId]
    b <- IdeaImages if b.id === id && !b.isRemoved
  } yield b

  def find(id: IdeaImageId): Option[IdeaImage] = byId(id).firstOption

  val byIdeaId = for {
    ideaId <- Parameters[IdeaId]
    b <- IdeaImages if b.ideaId === ideaId && !b.isRemoved
  } yield b

  def find(id: IdeaId): List[IdeaImage] = byIdeaId(id).list

  def findFirst(id: IdeaId): Option[IdeaImage] = byIdeaId(id).firstOption

  val byPath = for {
    path <- Parameters[String]
    b <- IdeaImages if b.path === path && !b.isRemoved
  } yield b

  def findByPath(path: String): Option[IdeaImage] = byPath(path).firstOption
}
