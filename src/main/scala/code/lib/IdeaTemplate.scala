package code.lib

import net.liftweb.common._
import net.liftweb.http._

import scala.xml.{Text, NodeSeq}

case class IdeaTemplate(
  name: String,
  title: String,
  description: NodeSeq,
  pages: List[String] = "index" :: Nil
) {
  def nextPage(page: String): Option[String] = pages.dropWhile(_ != page).drop(1).headOption
  def prevPage(page: String): Option[String] = pages.takeWhile(_ != page).lastOption
  def template(page: String): Box[NodeSeq] = pages.find(_ == page).flatMap(p => Templates("static" :: name :: p :: Nil))
  def screenshotPath: String = s"/static/$name/screenshot.png"
}

object IdeaTemplate extends Loggable {
  def all: List[IdeaTemplate] = List(
    IdeaTemplate("start123", "Start 123",
      Text("Simple template with no distractions that delivers clear message to the user."),
      "index" :: "start" :: Nil)
  , IdeaTemplate("denoizzed", "Denoizzed",
      Text("Beautiful high-contrast template with comprehensive messaging."),
      "index" :: "start" :: Nil)
  , IdeaTemplate("usability", "Usability",
      Text("Great battle-tested template by Craig Morrison allowing for elaborate content"),
      "index" :: "start" :: Nil)
  )

  def find(name: String): Option[IdeaTemplate] = all.find(_.name == name)
}

