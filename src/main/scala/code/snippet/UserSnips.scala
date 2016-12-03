package code.snippet

import code.lib._
import code.model.TokenType._
import code.model._
import net.liftweb.common._
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.sitemap.Loc.{LinkText, EarlyResponse}
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http._
import omniauth.Omniauth

import scala.xml.Text

class UserSignUp {
  var email = ""

  def render = {
    def sendTokenLink(user: User) = {
      val token = UserTokens.newToken(user.id, Activate)
      Mail.sendTokenEmail(user, token)
    }

    def process(): Unit = {
      Users.findByEmail(email) match {
        case Some(user) if user.isActive => // Do nothing
        case Some(user) if !user.isActive => sendTokenLink(user)
        case None =>
          val user = User(Some(email))
          User.validate.email.run(user) match {
            case e :: _ =>
              S.error("signup-msg", e.msg)
              S.redirectTo(Site.start.loc.calcDefaultHref)
            case Nil =>
              val newUser = Users.insertOrUpdate(user)
              sendTokenLink(newUser)
          }
      }
      S.notice(<span>An email with a secret link was just sent to <b>{email}</b></span>)
      S.redirectTo(Site.welcome.loc.calcDefaultHref)
    }

    ";email" #> SHtml.text(email, email = _) &
    ";submit" #> SHtml.onSubmitUnit(process)
  }
}

class UserForgotPassword {
  var email = ""

  def render = {
    def process(): Unit = {
      Users.findByEmail(email) match {
        case Some(user) if user.isActive =>
          val token = UserTokens.newToken(user.id, ResetPassword)
          Mail.sendTokenEmail(user, token)
        case None =>
      }

      S.notice(<span>An email with a secret link was just sent to <b>{email}</b></span>)
      S.redirectTo(Site.welcome.loc.calcDefaultHref)
    }

    ";email" #> SHtml.text(email, email = _) &
    ";submit" #> SHtml.onSubmitUnit(process)
  }
}

class UserActivate(token: UserToken) extends Loggable {
  var (password, confirmPassword, first, last) = ("", "", "", "")

  def render = {
    def process(): Unit = {
      if (password != confirmPassword) {
        S.error("activate-msg", "Password confirmation doesn't match")
      } else {
        Users.find(token.userId).map { user =>
          user.copy(password = password, first = first, last = last, isActive = true)
        } match {
          case None => S.redirectTo(Site.start.loc.calcDefaultHref)
          case Some(user) =>
            User.validate.user.run(user) match {
              case NamedError(_, msg) :: _ =>
                S.error("activate-msg", msg)
                S.redirectTo(S.uri)
              case _ =>
                val updatedUser = Users.insertOrUpdate(user.withPassword(password))
                UserTokens.deactivate(updatedUser.id, Activate)

                UserLogin.login(updatedUser, NextUri.get)

                S.notice("Your account is now active!")
                S.redirectTo(Site.start.loc.calcDefaultHref)
            }
        }
      }
    }

    ";first" #> SHtml.text(first, first = _) &
    ";last" #> SHtml.text(last, last = _) &
    ";password" #> SHtml.password(password, password = _) &
    ";confirm-password" #> SHtml.password(confirmPassword, confirmPassword = _) &
    ";submit" #> SHtml.onSubmitUnit(process)
  }
}

class ResetPassword(token: UserToken) extends Loggable {
  var (password, confirmPassword) = ("", "")

  def render = {
    def process(): Unit = {
      if (password != confirmPassword) {
        S.error("reset-password-msg", "Password confirmation doesn't match")
      } else {
        Users.find(token.userId).map { user =>
          user.copy(password = password)
        } match {
          case None => S.redirectTo(Site.start.loc.calcDefaultHref)
          case Some(user) =>
            User.validate.password.run(user) match {
              case e :: _ =>
                S.error("activate-msg", "Password should be at least 6 characters long")
                S.redirectTo(S.uri)
              case _ =>
                val updatedUser = Users.insertOrUpdate(user.withPassword(password))
                UserTokens.deactivate(updatedUser.id, ResetPassword)

                UserLogin.login(updatedUser, NextUri.get)

                S.notice("Password was successfully changed. You are now logged in!")
                S.redirectTo(Site.pages.loc.calcDefaultHref)
            }
        }
      }
    }

    ";password" #> SHtml.password(password, password = _) &
    ";confirm-password" #> SHtml.password(confirmPassword, confirmPassword = _) &
    ";submit" #> SHtml.onSubmitUnit(process)
  }
}

class UserLogin extends Loggable {
  var (email, password) = ("", "")

  val nextUri = NextUri.get

  def render = {
    def process(): Unit = {
      val uriBox = for {
        user <- Users.findByEmailAndActive(email, active = true).filter(_.checkPassword(password))
        uri <- UserLogin.login(user, nextUri)
      } yield uri

      uriBox match {
        case Some(uri) => S.redirectTo(uri)
        case None =>
          S.error("login-msg", "Invalid email or password")
          S.redirectTo(S.uri)
      }
    }

    ";email" #> SHtml.text(email, email = _) &
    ";password" #> SHtml.password(password, password = _) &
    ";submit" #> SHtml.onSubmitUnit(process)
  }

  def social = {
    ";twitter [onclick]" #> SHtml.ajaxInvoke { () =>
      S.redirectTo("/auth/twitter/signin")
    } &
    ";facebook [onclick]" #> SHtml.ajaxInvoke { () =>
      S.redirectTo("/auth/facebook/signin")
    }
  }
}

object UserLogin extends Loggable {
  val socialLoginResponse = EarlyResponse { () =>
    val authBox = Omniauth.currentAuth
    Omniauth.clearCurrentAuth

    val uri = (for {
      auth <- authBox
      provider <- AuthInfoProvider.findOption(auth.provider)
    } yield (auth, provider)) map { case (auth, provider) =>
      (for {
        authInfo <- UserAuthInfos.findByUidAndProvider(auth.uid, provider)
        user <- Users.find(authInfo.userId)
      } yield UserWithAuthInfo(user, Some(authInfo))).getOrElse {
        val first = auth.firstName.orElse(auth.name.split(" ").headOption).getOrElse("")
        val last = auth.lastName.getOrElse(auth.name.split(" ").toList.tail.mkString(" "))
        val user = Users.insertOrUpdate(User(None, first = first, last = last, isActive = true))
        val authInfo = UserAuthInfos.insertOrUpdate {
          UserAuthInfo(provider, uid = auth.uid, nickname = auth.nickName, userId = user.id)
        }
        UserWithAuthInfo(user, Some(authInfo))
      }
    } flatMap { userWithAuthInfo =>
      UserLogin.login(userWithAuthInfo, Empty)
    } match {
      case Full(u) => S.mapFuncToURI(u, () => S.notice("You are now logged in."))
      case _ => S.mapFuncToURI(Site.login.loc.calcDefaultHref, () => S.error("Login failed. Please try again."))
    }

    Full(RedirectResponse(uri))
  }

  def login(user: User, nextUri: Box[String]): Box[String] = {
    login(UserWithAuthInfo(user, None), nextUri)
  }

  def login(userWithAuthInfo: UserWithAuthInfo, nextUri: Box[String]): Box[String] = {
    Full(userWithAuthInfo).filter(_.user.isActive).pass { userWithAuthInfoBox =>
      CurrentUser.apply(userWithAuthInfoBox)
    }.map { _ =>
      // get new uri
      (TryCreateIdea.get, nextUri) match {
        case (Full(iwd), _) =>
          val template = TryCreateIdea.template
          S.mapFuncToURI(Site.create.calcHref((template, "index")), () => {
            CurrentIdea.apply(TryCreateIdea.get)
            TryCreateIdea.apply(Empty)
          })
        case (_, Full(uri)) => uri
        case _ => Site.pages.loc.calcDefaultHref
      }
    }
  }
}

class UserProfile {
  var first = CurrentUser.get.map(_.user.first).openOrThrowException("should be authenticated")
  var last = CurrentUser.get.map(_.user.last).openOrThrowException("should be authenticated")

  def render = {
    def process(): Unit = {
      CurrentUser.get.flatMap { uai =>
        val newUser = Users.insertOrUpdate(uai.user.copy(first = first, last = last))
        UserLogin.login(uai.copy(user = newUser), Full(Site.profile.loc.calcDefaultHref))
      } match {
        case Full(uri) =>
          S.notice("Your profile has been successfully updated")
          S.redirectTo(uri)
        case _ => S.redirectTo(Site.start.loc.calcDefaultHref)
      }
    }
    ";first" #> SHtml.text(first, first = _) &
    ";last" #> SHtml.text(last, last = _) &
    ";submit" #> SHtml.onSubmitUnit(process)
  }
}

object UserProfile {
  val linkText = LinkText[Unit](_ => {
    CurrentUser.get match {
      case Full(UserWithAuthInfo(user, Some(info))) if info.provider == AuthInfoProvider.Twitter =>
        <span><i class="fa fa-twitter"></i> {user.name}</span>
      case Full(UserWithAuthInfo(user, Some(info))) if info.provider == AuthInfoProvider.Facebook =>
        <span><i class="fa fa-facebook"></i> {user.name}</span>
      case Full(UserWithAuthInfo(user, _)) => Text(user.name)
      case _ => Text("Profile")
    }
  })
}

object UserLogout {
  val earlyResponse = EarlyResponse(() => {
    CurrentUser.set(Empty)
    Full(RedirectResponse(Site.start.loc.calcDefaultHref))
  })

  val linkText = LinkText[Unit](_ => <i class="fa fa-sign-out"></i>)
}