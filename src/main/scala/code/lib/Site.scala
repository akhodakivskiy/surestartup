package code.lib

import java.io.{OutputStream, OutputStreamWriter, BufferedWriter}
import java.nio.file.{Paths, Path, Files}

import code.model._
import code.snippet._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap.Menu.{ParamMenuable, Menuable}
import net.liftweb.sitemap._
import net.liftweb.util._
import omniauth.Omniauth

import scala.xml.Text

object Site extends Loggable {
  def scheme: String = S.request.map(_.request.scheme).openOr("https")
  def port: String = S.request.map(_.request.serverPort).filter(_ != 80).map(p => s":$p").openOr("")

  val tld = Props.get("project.tld", "localhost")
  val name = Props.get("project.name", "SureStartup")

  val AuthGroup = LocGroup("auth")
  val NavGroup = LocGroup("nav")
  val AdminGroup = LocGroup("admin")
  val AdminPagesGroup = LocGroup("admin-pages")

  val RequireAnon = If(
    () => !S.loggedIn_?,
    () => RedirectResponse(pages.loc.calcDefaultHref))

  val RequireAuth = If(
    () => S.loggedIn_?,
    () => {
      val uri = S.uriAndQueryString
      RedirectWithState(login.loc.calcDefaultHref, RedirectState(() => NextUri(uri)))
    })

  val RequireAdmin = If(
    () => CurrentUser.get.exists(_.user.isAdmin),
    () => RedirectResponse(pages.loc.calcDefaultHref))

  val RequireTLD = TestAccess(() => S.hostName match {
    case `tld` => Empty
    case _ => Full(NotFoundResponse(""))
  })

  val RequireDomain = TestValueAccess[(Idea, String)] {
    case Full((idea, page)) if idea.matchesHost(S.hostName, tld) => Empty
    case _ => Full(NotFoundResponse(""))
  }

  val RequireTryCreateIdeaVar = TestAccess(() => TryCreateIdea.get match {
    case Full(_) => Empty
    case _ => Full(RedirectResponse(Site.start.loc.calcDefaultHref))
  })

  val RequireCreateIdeaVar = TestAccess(() => CurrentIdea.get match {
    case Full(_) => Empty
    case _ => Full(RedirectResponse(Site.pages.loc.calcDefaultHref))
  })

  val RequireEditIdeaVar = TestValueAccess[(Idea, String)] {
    case Full((idea, _)) if CurrentIdea.get.exists(_.idea.id == idea.id) => Empty
    case Full((idea, _)) => Full(RedirectResponse(dashboard.calcHref(idea)))
    case _ => Full(NotFoundResponse(""))
  }

  val CalcFromHost = CalcValue { () =>
    val host: String = S.hostName.replaceFirst(s".$tld$$", "")
    val page: String = S.request.flatMap(_.path.partPath.headOption).openOr("index")
    logger.info(s"$host $page")
    Ideas.findByHost(host).filter(_.isPublished).map((_, page))
  }

  val TryCreateTemplateFinder = ValueTemplateBox[String] {
    case Full(page) => TryCreateIdea.templateBox.flatMap(_.template(page))
    case _ => Empty
  }

  val CreateTemplateFinder = ValueTemplateBox[(IdeaTemplate, String)] {
    case Full((template, page)) => template.template(page)
    case _ => Empty
  }

  val IdeaTemplateFinder = ValueTemplateBox[(Idea, String)] {
    case Full((idea, page)) => IdeaTemplate.find(idea.template).flatMap(_.template(page))
    case _ => Empty
  }

  val TryCreateControlsSnippets = ValueSnippets[String] {
    case ("Controls", Full(page)) =>
      new TryCreateControls(TryCreateIdea.template, page).render
  }

  val CreateControlsSnippets = ValueSnippets[(IdeaTemplate, String)] {
    case ("Controls", Full((template, page))) => new CreateControls(template, page).render
  }

  val EditControlsSnippets = ValueSnippets[(Idea, String)] {
    case ("Controls", Full((idea, page))) => new EditControls(idea, page).render
  }

  val TryCreateIdeaSnippets = new DispatchLocSnippets {
    def dispatch = new EditIdea(TryCreateIdea).dispatch
  }

  val EditIdeaSnippets = new DispatchLocSnippets {
    def dispatch = new EditIdea(CurrentIdea).dispatch
  }

  val tryCreate = Menu.param[String](
    "Try Create", "Try Create", page => Full(page), identity
  ) / "try" / * >> RequireAnon >> RequireTLD >> NavGroup >>
    TryCreateTemplateFinder >> TryCreateIdeaSnippets >> TryCreateControlsSnippets >> RequireTryCreateIdeaVar

  val create = Menu.params[(IdeaTemplate, String)](
    "Create", "Create", {
      case template :: page :: Nil => IdeaTemplate.find(template).map(t => (t, page))
      case _ => Empty
    },
    p => p._1.name :: p._2 :: Nil
  ) / "create" / * / * >> RequireAuth >> RequireTLD >>
    CreateTemplateFinder >> EditIdeaSnippets >> CreateControlsSnippets >> RequireCreateIdeaVar

  val edit = Menu.params[(Idea, String)](
    "Edit", "Edit", {
      case host :: page :: Nil => Ideas.findByHost(host).map((_, page))
      case _ => Empty
    },
    p => p._1.localId :: p._2 :: Nil
  ) / "edit" / * / * >> RequireAuth >> RequireTLD >>
    IdeaTemplateFinder >> EditIdeaSnippets >> EditControlsSnippets >> RequireEditIdeaVar

  val index = Menu.param[(Idea, String)](
    "Index", "Index",
    page => calcFromHost(page),
    _._2
  ) / * >> RequireDomain >> IdeaTemplateFinder

  def calcFromHost(page: String): Option[(Idea, String)] = {
    val host: String = S.hostName.replaceFirst(s".${tld}$$", "")
    if (host == tld && page != "index") {
      None
    } else {
      Ideas.findByHost(host).filter(_.isPublished).map((_, page))
    }
  }

  lazy val login: Menuable = Menu.i("Log In") / "login" >> RequireAnon >> RequireTLD >> AuthGroup
  lazy val signup: Menuable = Menu.i("Sign Up") / "signup" >> RequireAnon >> RequireTLD >> AuthGroup >> MenuCssClass("navbar-signup")
  lazy val forgotPassword: Menuable = Menu.i("Forgot Password") / "forgot-password" >> RequireAnon >> RequireTLD
  lazy val socialLogin: Menuable = Menu.i("Social Log In") / "login" / "social" >>
    RequireAnon >> RequireTLD >> UserLogin.socialLoginResponse

  lazy val start: Menuable = Menu.i("Get Started") / "start" >> RequireAnon >> RequireTLD >> AuthGroup submenus tryCreate
  lazy val welcome: Menuable = Menu.i("Welcome") / "welcome" >> RequireAnon >> RequireTLD
  lazy val profile: Menuable = Menu(
    "Profile", UserProfile.linkText
  ) / "profile" >> RequireAuth >> RequireTLD >> AuthGroup
  lazy val logout: Menuable = Menu(
    "Log Out", UserLogout.linkText
  ) / "logout" >> RequireAuth >> RequireTLD >> AuthGroup >> UserLogout.earlyResponse
  lazy val activate = Menu.param[UserToken](
    "Activate", "Activate",
    token => UserTokens.find(token, TokenType.Activate),
    _.token
  ) / "activate" >> RequireAnon >> RequireTLD

  lazy val resetPassword = Menu.param[UserToken](
    "Reset Password", "Reset Password",
    token => UserTokens.find(token, TokenType.ResetPassword),
    _.token
  ) / "reset-password" >> RequireAnon >> RequireTLD

  lazy val dashboard = Menu.param[Idea](
    "Dashboard", "Dashboard",
    strId => Ideas.find(strId),
    _.id.toString
  ) / "dashboard" >> RequireAuth >> RequireTLD

  lazy val signups = Menu.param[Idea](
    "Signups", "Signups",
    id => Ideas.find(id),
    _.id.toString
  ) / "signups" >> RequireAuth >> RequireTLD submenus signupsCsv

  lazy val signupsCsv = Menu.param[Idea](
    "Signups CSV", "Signups CSV",
    id => Ideas.find(id),
    _.id.toString
  ) / "signups-csv" >> RequireAuth >> RequireTLD >> EarlyResponse(() => {
    S.location.flatMap(_.currentValue).map(_.asInstanceOf[Idea]).map { idea =>
      val headers = List(
        "Content-Transfer-Encoding" -> "binary",
        "Content-Disposition" -> s"""attachment; filename="signups-${idea.id}.csv";""",
        "Content-Type" -> "application/octet-stream"
      )
      val printer: OutputStream => Unit = { out =>
        val signups = IdeaSignups.find(idea.id)
        val writer = new BufferedWriter(new OutputStreamWriter(out))
        signups.csv().foreach { line =>
          writer.write(line)
          writer.newLine()
        }
        writer.close()
      }
      OutputStreamResponse(printer, headers)
    }
  })

  lazy val newPage: Menuable = Menu.i("New Page") / "new-page" >> RequireTLD >> NavGroup >> RequireAuth submenus create
  lazy val pages: Menuable = Menu.i("Pages") / "pages" >> RequireTLD >> NavGroup >> RequireAuth submenus (dashboard, edit, signups)

  object admin {
    val adminUsers = Menu.i("Admin Users") / "admin" / "users" >> RequireAdmin >> RequireTLD >> AdminPagesGroup
    val adminPages = Menu.i("Admin Pages") / "admin" / "pages" >> RequireAdmin >> RequireTLD >> AdminPagesGroup

    val index = Menu.i("Admin") / "admin" >> RequireAdmin >> RequireTLD >> AdminGroup submenus(adminUsers, adminPages)
  }

  def boot() = {
    val menus = Omniauth.sitemap ::: List(
      start,
      welcome,
      pages,
      newPage,
      login,
      signup,
      forgotPassword,
      socialLogin,
      profile,
      logout,
      activate,
      resetPassword,
      admin.index,
      index)
    val siteMap = SiteMap(menus: _*)
    LiftRules.setSiteMap(siteMap)
    LiftRules.loggedInTest = Full(() => CurrentUser.get.exists(_.user.isActive))

    Omniauth.init
  }
}
