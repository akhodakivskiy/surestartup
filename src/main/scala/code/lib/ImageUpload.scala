package code.lib

import java.awt.image.BufferedImage
import java.io.InputStream
import java.nio.file.{StandardCopyOption, Path, Files, Paths}
import javax.imageio.ImageIO

import code.model.IdeaImage
import code.rest.{ImageDownloadRest, ImageUploadRest}
import net.liftweb.common.{Loggable, Full}
import net.liftweb.http.{S, LiftRules}
import net.liftweb.util.Props
import org.imgscalr.Scalr

/**
 * Created by anton on 11/9/14.
 */
trait ImageUpload {
  def url(path: Path): String
  def permPath(image: IdeaImage): Path
  def tempPath(id: String): Path

  def save(input: InputStream, id: String, width: Int, height: Int): Unit
  def copy(from: Path, to: Path): Unit

  def isPerm(image: IdeaImage): Boolean = Paths.get(image.path).equals(permPath(image))
  def isTemp(image: IdeaImage): Boolean = !isPerm(image)

  def makeThumbnail(image: BufferedImage, width: Int, height: Int): BufferedImage = {
    Scalr.resize(image, Scalr.Method.QUALITY, Scalr.Mode.FIT_TO_WIDTH, width, height)
  }

  def boot(): Unit
}

object ImageUpload extends ImageUpload with Loggable {
  private val instance: ImageUpload = Props.get("idea.images") match {
    case Full("filesystem") => FileSystemImageUpload
    case other => throw new RuntimeException(s"idea.images value is invalid: $other")
  }

  def url(path: Path): String = instance.url(path)
  def permPath(image: IdeaImage): Path = instance.permPath(image)
  def tempPath(id: String): Path = instance.tempPath(id)

  def save(input: InputStream, id: String, width: Int, height: Int): Unit = instance.save(input, id, width, height)
  def copy(from: Path, to: Path): Unit = instance.copy(from, to)

  def boot(): Unit = {
    LiftRules.dispatch.append(ImageUploadRest)
    instance.boot()
  }
}

object FileSystemImageUpload extends ImageUpload {
  val tempDir = System.getProperty("java.io.tmpdir")
  val imagesDir = Paths.get(Props.get("idea.images.filesystem.dir", tempDir))

  def fileSystemPath(path: Path): Path = imagesDir.resolve(path)
  def tempFileSystemPath(id: String): Path = fileSystemPath(tempPath(id))
  def permFileSystemPath(image: IdeaImage): Path = fileSystemPath(permPath(image))

  def url(path: Path): String = List(S.hostAndPath, "images", path.toString).mkString("/")
  def permPath(image: IdeaImage): Path = Paths.get(image.ideaId.toString, image.id.toString, image.name)
  def tempPath(id: String): Path = Paths.get("tmp", id)

  def save(input: InputStream, id: String, width: Int, height: Int): Unit = {
    val image = makeThumbnail(ImageIO.read(input), width, height)

    val path = tempFileSystemPath(id)

    Files.createDirectories(path.getParent)
    ImageIO.write(image, "png", path.toFile)
  }

  def copy(from: Path, to: Path): Unit = {
    val fsFrom = fileSystemPath(from)
    val fsTo = fileSystemPath(to)
    Files.createDirectories(fsTo.getParent)
    Files.copy(fsFrom, fsTo, StandardCopyOption.REPLACE_EXISTING)
  }

  def boot(): Unit = LiftRules.dispatch.append(ImageDownloadRest)
}
