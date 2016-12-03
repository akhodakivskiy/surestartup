package code.rest

import java.nio.file.{Paths, Files}

import code.lib.{UploadInfo, UploadInfoRegistry, FileSystemImageUpload, ImageUpload}
import code.model.{IdeaId, IdeaImageId, IdeaImages, IdeaImage}
import net.liftweb.common.{Full, Loggable}
import net.liftweb.http._
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JsonAST._
import net.liftweb.util.Props
import net.liftweb.util.Helpers._

object ImageUploadRest extends RestHelper with Loggable {
  serve {
    case "image_upload" :: Nil Post req => for {
      id: String <- req.param("id")
      info: UploadInfo <- UploadInfoRegistry.find(id)

      file <- req.uploadedFiles.headOption
    } yield {
      ImageUpload.save(file.fileStream, id, info.width, info.height)
      JsonResponse(JObject(JField("success", JBool(true)) :: Nil))
    }
  }
}

object AsIdeaId {
  def unapply(in: String): Option[IdeaId] = asLong(in).map(IdeaId.apply)
}

object AsIdeaImageId {
  def unapply(in: String): Option[IdeaImageId] = asLong(in).map(IdeaImageId.apply)
}

object ImageDownloadRest extends RestHelper with Loggable {
  serve {
    case "images" :: "tmp" :: id :: Nil Get req => {
      Full(FileSystemImageUpload.tempFileSystemPath(id)).filter(p => Files.exists(p)) map { path =>
        OutputStreamResponse { os: java.io.OutputStream =>
          Files.copy(path, os)
        }
      }
    }
    case "images" :: AsIdeaId(ideaId) :: AsIdeaImageId(id) :: name :: Nil Get req => {
      IdeaImages.find(id).filter(i => i.ideaId == ideaId && i.name == name).map { image =>
        val path = FileSystemImageUpload.permFileSystemPath(image)
        OutputStreamResponse { os: java.io.OutputStream =>
          Files.copy(path, os)
        }
      }
    }
  }
}
