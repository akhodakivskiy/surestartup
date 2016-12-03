package code.lib

import java.nio.file.Paths

import code.model._
import net.liftweb.common._
import net.liftweb.http.{SessionVar, RequestVar}
import net.liftweb.util.Helpers._

case class UserWithAuthInfo(user: User, authInfo: Option[UserAuthInfo])

object CurrentUser extends SessionVar[Box[UserWithAuthInfo]](Empty) {
  def update(first: String, last: String): Box[UserWithAuthInfo] = {
    update(_.map(uai => uai.copy(user = uai.user.copy(first = first, last = last))))
  }
}

object NextUri extends RequestVar[Box[String]](Empty)

case class UploadInfo(id: String, width: Int, height: Int)

object UploadInfoRegistry extends SessionVar[List[UploadInfo]](Nil) {
  def add(id: String, width: Int, height: Int): List[UploadInfo] = update(UploadInfo(id, width, height) :: _)
  def find(id: String): Option[UploadInfo] = get.find(_.id == id)
}

case class IdeaWithData(idea: Idea, texts: List[IdeaText], images: List[IdeaImage])

object IdeaWithData {
  val empty = IdeaWithData(Idea.empty, Nil, Nil)
}

object CurrentIdea extends RequestVar[Box[IdeaWithData]](Empty) with IdeaEditor

object TryCreateIdea extends SessionVar[Box[IdeaWithData]](Empty) with IdeaEditor

trait IdeaEditor {
  def apply(v: Box[IdeaWithData]): Box[IdeaWithData]
  def update(f: Box[IdeaWithData] => Box[IdeaWithData]): Box[IdeaWithData]
  def get: Box[IdeaWithData]
  
  def load(idea: Idea): Unit = {
    val (texts, images) = idea.id match {
      case IdeaId.empty => (Nil, Nil)
      case _ => (IdeaTexts.find(idea.id), IdeaImages.find(idea.id))
    }
    apply(Full(IdeaWithData(idea, texts, images)))
  }

  def templateBox: Box[IdeaTemplate] = get.map(_.idea.template).flatMap(IdeaTemplate.find)
  def template: IdeaTemplate = templateBox.openOrThrowException("var is not full or template is wrong")

  def idea: Idea = get.openOrThrowException("var is not Full").idea
  def texts: List[IdeaText] = get.toList.flatMap(_.texts)
  def images: List[IdeaImage] = get.toList.flatMap(_.images)

  def updateFull(func: (IdeaWithData) => IdeaWithData): Box[IdeaWithData] = update(_.map(func))

  def updateIdea(func: (Idea) => Idea) = updateFull(iwt => iwt.copy(idea = func(iwt.idea)))

  def text(name: String): Option[IdeaText] = get.flatMap(_.texts.find(_.name == name))
  def textIndex(name: String): Option[Int] = get.flatMap(_.texts.zipWithIndex.find(_._1.name == name).map(_._2))
  def addText(text: IdeaText): Unit = updateFull(iwt => iwt.copy(texts = iwt.texts :+ text))
  def updateText(name: String, text: String): Unit = updateText(name, _.copy(text = text), Some(IdeaText(name, text)))
  def updateText(name: String, func: (IdeaText) => IdeaText, dflt: Option[IdeaText] = None): Unit = updateFull { iwt =>
    textIndex(name) match {
      case Some(idx) => iwt.copy(texts = texts.replace(idx, func(texts(idx))))
      case None => dflt match {
        case Some(default) => iwt.copy(texts = texts :+ default)
        case None => iwt
      }
    }
  }

  def image(name: String): Option[IdeaImage] = get.flatMap(_.images.find(_.name == name))
  def imageIndex(name: String): Option[Int] = get.flatMap(_.images.zipWithIndex.find(_._1.name == name).map(_._2))
  def addImage(image: IdeaImage): Unit = updateFull(iwt => iwt.copy(images = iwt.images :+ image))
  def updateImage(name: String, path: String, width: Int, height: Int): Unit = {
    updateImage(name, _.copy(path = path, width = width, height = height), Some(IdeaImage(name, path, width, height)))
  }
  def updateImage(name: String, func: (IdeaImage) => IdeaImage, dflt: Option[IdeaImage] = None): Unit = updateFull {iwt =>
    imageIndex(name) match {
      case Some(idx) => iwt.copy(images = images.replace(idx, func(images(idx))))
      case None => dflt match {
        case Some(default) => iwt.copy(images = images :+ default)
        case None => iwt
      }
    }
  }

  def persist() = Slick.db.withDynTransaction {
    updateIdea { idea =>
      if (idea.localId.isEmpty) {
        idea.copy(localId = Ideas.nextLocalId(idea.title))
      } else {
        idea
      }
    }
    updateIdea(Ideas.insertOrUpdate)
    this.texts.foreach { text =>
      updateText(text.name, t => IdeaTexts.insertOrUpdate(t.copy(ideaId = idea.id)))
    }
    this.images.foreach { image =>
      updateImage(image.name, i => {
        val newImage = IdeaImages.insertOrUpdate(i.copy(ideaId = idea.id))
        if (ImageUpload.isTemp(newImage)) {
          val from = Paths.get(newImage.path)
          val to = ImageUpload.permPath(newImage)
          ImageUpload.copy(from, to)
          IdeaImages.insertOrUpdate(newImage.copy(path = to.toString))
        } else {
          newImage
        }
      })
    }
  }
}

