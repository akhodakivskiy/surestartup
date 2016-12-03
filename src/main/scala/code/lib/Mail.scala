package code.lib

import javax.mail.{Authenticator, PasswordAuthentication}

import code.model.{TokenType, UserToken, User, Idea}
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Mailer._
import net.liftweb.util._
import net.liftweb.util.Helpers._

import scala.xml.NodeSeq

/**
 * Created by anton on 10/28/14.
 */
object Mail extends Loggable {

  val noReply = s"${Site.name} <no-reply@${Site.tld}>"

  def boot() = {
    (Props.getBool("mail.smtp.auth"), Props.get("mail.user"), Props.get("mail.password")) match {
      case (Full(true), Full(user), Full(password)) =>
        Mailer.authenticator = Full(new Authenticator() {
          override def getPasswordAuthentication = new PasswordAuthentication(user, password)
        })
      case _ => throw new RuntimeException("username/password not provided for Mailer")
    }
  }

  def sendMail(from: String, to: String, subject: String, replyTo: String,
               path: List[String], snips: (String, NodeSeq => NodeSeq)*): Unit = {
    S.runTemplate(path, snips: _*) match {
      case Full(html) => sendMail(from, to, subject, replyTo, html)
      case box => logger.error(box)
    }
  }

  def sendMail(from: String, to: String, subject: String, replyTo: String, html: NodeSeq): Unit = {
    val headers: List[MessageHeader] = (for {
      str <- Props.get("mail.headers").toList
      name <- str.split(",").toList
      value <- Props.get(s"mail.headers.$name")
    } yield MessageHeader(name, value)).toList

    val mailTypes: List[MailTypes] =
      PlainMailBodyType(html.text) ::
      XHTMLMailBodyType(html) ::
      To(to) ::
      ReplyTo(replyTo) :: headers

    Mailer.sendMail(From(from), Subject(subject), mailTypes: _*)
  }

  implicit class UserTokenForEmail(token: UserToken) {
    def url: String = S.hostAndPath + uri

    def uri: String = token.tokenType match {
      case TokenType.Activate => Site.activate.calcHref(token)
      case TokenType.ResetPassword => Site.resetPassword.calcHref(token)
      case _ => throw new RuntimeException(s"unexpected token type: ${token.tokenType}")
    }

    def template: List[String] = token.tokenType match {
      case TokenType.Activate => "emails-hidden" :: "activate" :: Nil
      case TokenType.ResetPassword => "emails-hidden" :: "reset-password" :: Nil
      case _ => throw new RuntimeException(s"unexpected token type: ${token.tokenType}")
    }

    def subject: String = token.tokenType match {
      case TokenType.Activate => s"${Site.name} - Welcome"
      case TokenType.ResetPassword => s"${Site.name} - Reset Password"
      case _ => throw new RuntimeException(s"unexpected token type: ${token.tokenType}")
    }
  }

  def sendTokenEmail(user: User, token: UserToken): Unit = {
    user.email.foreach { email =>
      val snip = "EmailUrl" -> ("^ [href]" #> token.url & "^ *" #> token.url)
      sendMail(noReply, email, token.subject, noReply, token.template, snip)
    }
  }
}
