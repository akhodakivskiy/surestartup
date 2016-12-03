package code.snippet

import java.nio.file.Paths

import code.lib._
import code.model._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsExp._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJE._
import net.liftweb.util._
import net.liftweb.util.Helpers._

import scala.xml._

case class Select(id: String) extends JsCmd {
  def toJsCmd = "if (document.getElementById(" + id.encJs + ")) {document.getElementById(" + id.encJs + ").select();};"
}

object ReturnFalse extends JsCmd {
  def toJsCmd = "return false;"
}

object JsEvent {
  def apply(): JsExp = JsVar("(arguments[0] || window.event)")
  def preventDefault: JsExp = apply() ~> JsFunc("preventDefault")
}

class EditIdea(editor: IdeaEditor) extends DispatchSnippet with Loggable {
  def dispatch = {
    case "IdeaTitle" => title
    case "IdeaDescription" => description
    case "IdeaText" => text
    case "IdeaImg" => image
    case "IdeaGA" => PassThru
    case "IdeaEmailSignup" => signup
    case "IdeaMeta" => meta
    case "IdeaPowered" => ClearNodes
  }

  def title = ViewEditSnips.textField(() => editor.idea.title, v => editor.updateIdea(_.copy(title = v)))

  def description = ViewEditSnips.textField(() => editor.idea.description, v => editor.updateIdea(_.copy(description = v)))

  def text = {
    val name = S.attr("name").openOrThrowException("name attr is mandatory")

    ViewEditSnips.textAreaField(() => editor.text(name).map(_.text).getOrElse(""), v => editor.updateText(name, v))
  }

  def image = {
    val name = S.attr("name").openOrThrowException("name attr is mandatory")
    val width = S.attr("width").flatMap(asInt).openOrThrowException("name attr is mandatory")
    val height = S.attr("height").flatMap(asInt).openOrThrowException("name attr is mandatory")
    val id = nextFuncName

    UploadInfoRegistry.add(id, width, height)

    def uploaded(): JsCmd = {
      val path = ImageUpload.tempPath(id)
      editor.updateImage(name, path.toString, width, height)
      editor.image(name) match {
        case Some(image) => Jq(s"#$id img") ~> JqAttr("src", ImageUpload.url(Paths.get(image.path)))
        case None => Noop
      }
    }

    S.appendJs {
      JqId(id) ~> JsFunc("dropzone", JsObj(
        "url" -> Str("/image_upload"),
        "previewsContainer" -> JsFalse,
        "clickable" -> Str(s"#$id img"),
        "params" -> JsObj("id" -> Str(id)),
        "error" -> AnonFunc("file, resp, req", Noop),
        "success" -> AnonFunc("file, resp, req", SHtml.ajaxInvoke(uploaded))
      ))
    }

    "^ [id]" #> id &
    "img [class+]" #> "surestartup-image-border" & {
      editor.image(name) match {
        case Some(image) => "img [src]" #> ImageUpload.url(Paths.get(image.path))
        case None => "foo" #> PassThru
      }
    }
  }

  def signup = {
    ";submit" #> { ns: NodeSeq =>
      ns.map {
        case e: Elem if e.label == "button" => e.copy(label = "a")
        case e => e
      }
    } &
    ";feedback [data-lift]" #> "IdeaText?name=signup-feedback" &
    ";feedback" #> { node: NodeSeq =>
      node ++
      <p class="surestartup-signup-help">
        The message above will be shown to the user after the form is submitted.
        This message will not be shown to the user.
      </p>
    }
  }

  def meta(ns: NodeSeq): NodeSeq = {
    new IdeaMeta((editor.idea, "")).render(ns)
  }
}

class IdeaTitle(arg: (Idea, String)) {
  def render = "^ *" #> arg._1.title
}

class IdeaDescription(arg: (Idea, String)) {
  def render = "^ *" #> arg._1.description
}

class IdeaText(arg: (Idea, String)) {
  val texts = IdeaTexts.find(arg._1.id)

  def render = {
    val name = S.attr("name").openOrThrowException("name attr is mandatory")
    "span" #> texts.find(_.name == name).map(_.text).getOrElse("--missing value--")
  }
}

class IdeaImg(arg: (Idea, String)) {
  val (idea, page) = arg
  val images = IdeaImages.find(idea.id)

  def render = {
    val name = S.attr("name").openOrThrowException("name attr is mandatory")
    images.find(_.name == name) match {
      case None => PassThru
      case Some(image) => "img [src]" #> ImageUpload.url(Paths.get(image.path))
    }
  }
}

class IdeaGA(arg: (Idea, String)) {
  val (idea, page) = arg
  val enable = Props.getBool("project.ga", defVal = false)

  def render = {
    (enable, idea.ga) match {
      case (false, _) => "^" #> ClearNodes
      case (true, Some(code)) =>
        "^ *+" #> s"""ga('create', '$code', 'auto', {'name': 'userTracker'});
                     |ga('userTracker.send', 'pageview');""".stripMargin
      case _ => PassThru
    }
  }
}

class IdeaEmailSignup(arg: (Idea, String)) {
  val (idea, page) = arg

  val feedbackId = nextFuncName
  val submitId = nextFuncName

  var email = ""
  var name = ""
  var message = ""

  def render(node: NodeSeq): NodeSeq = {

    def process(): JsCmd = {
      IdeaSignups.insertOrUpdate(IdeaSignup(email, name, message, ideaId = idea.id))
      JsShowId(feedbackId) & JsHideId(submitId)
    }

    val bindForm = {
      ";email" #> SHtml.text(email, email = _) &
      ";name" #> SHtml.text(name, name = _) &
      ";message" #> SHtml.textarea(message, message = _) &
      ";submit" #> SHtml.ajaxOnSubmit(process) &
      ";submit [id]" #> submitId &
      ";feedback [style]" #> "display:none" &
      ";feedback [id]" #> feedbackId
    }

    SHtml.ajaxForm(bindForm(node))
  }
}

class IdeaMeta(arg: (Idea, String)) {
  val (idea, page) = arg

  val imageOpt = IdeaImages.findFirst(idea.id)

  def render(ns: NodeSeq): NodeSeq = {
    <title>{idea.title}</title>
    <meta name="title" content={idea.title} />
    <meta name="description" content={idea.description} />
    <meta name="og:title" value={idea.title} />
    <meta name="og:site_name" value={idea.title} />
    <meta name="og:type" value="website" />
    <meta name="og:description" value={idea.description} /> ++
    imageOpt.map { image =>
      val path = Paths.get(image.path)
      <meta property="og:image" value={ImageUpload.url(path)}/>
    }.getOrElse(NodeSeq.Empty)
  }
}

class IdeaPowered(arg: (Idea, String)) {
  def render = {
    val scheme = S.request.map(_.request.scheme).getOrElse("http")
    val tld = Site.tld
    val port = S.request.map(":" + _.request.serverPort).getOrElse("")
    "a [href]" #> s"$scheme://$tld$port"
  }
}

object ViewEditSnips extends Loggable {
  type EditorFactory = (String, String, (String) => JsCmd) => Elem

  def textAreaField(get: () => String, set: (String) => Any) = field(get, set, (id, g, s) => {
    SHtml.ajaxTextarea(g, s, "id" -> id, "class" -> "surestartup-edit", "rows" -> "3")
  })

  def textField(get: () => String, set: (String) => Any) = field(get, set, (id, g, s) => {
    SHtml.ajaxText(g, s, "id" -> id, "class" -> "surestartup-edit")
  })

  def field(get: () => String, set: (String) => Any, editorFactory: EditorFactory) = SHtml.idMemoize { node =>
    val titleId = nextFuncName

    "span *" #> { in: NodeSeq =>
      if (get().length == 0) {
        set(in.text)
      }
      get()
    } &
    "^ [id]" #> titleId &
    "span [class+]" #> "surestartup-edit-border" &
    "span [onclick]" #> SHtml.ajaxInvoke { () =>
      val editor = editorFactory(titleId, get(), { newValue: String =>
        set(newValue.trim)
        node.setHtml()
      })
      Replace(titleId, editor) & Focus(titleId) & Select(titleId)
    } &
    "a [onclick]" #> (JsEvent.preventDefault & ReturnFalse) &
    "a [rel]" #> ""
  }
}
