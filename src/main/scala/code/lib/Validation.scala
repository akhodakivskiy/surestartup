package code.lib

import java.util.regex.Pattern

import net.liftweb.common.Loggable
import net.liftweb.http.S
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.util.Helpers

import scala.util.matching.Regex
import scala.xml.NodeSeq

trait ValError {
  def msg: NodeSeq
}

case class AnonError(msg: NodeSeq) extends ValError
case class NamedError(name: String, msg: NodeSeq) extends ValError

case class Validator[T](run: (T) => List[ValError]) {
  def &(other: Validator[T]): Validator[T] = Validator { t: T =>
    run(t) ::: other.run(t)
  }
}

object Validator extends Loggable {
  def apply[T](name: String, msg: NodeSeq)(func: (T) => Boolean): Validator[T] = Validator { t: T =>
    if (!func(t)) NamedError(name, msg) :: Nil else Nil
  }

  def notEmpty[T](name: String, msg: NodeSeq)(func: (T) => String): Validator[T] = Validator[T](name, msg)(!func(_).isEmpty)

  def regex[T](name: String, msg: NodeSeq)(re: Regex, func: (T) => String): Validator[T] = Validator[T](name, msg) { t =>
    re.findFirstIn(func(t)).isDefined
  }

  def email[T](name: String, msg: NodeSeq)(func: (T) => String): Validator[T] = {
    regex(name, msg)("""\b([a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+)@([a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*)\b""".r, func)
  }

  def minLength[T](name: String, msg: NodeSeq)(len: Int, func: (T) => String): Validator[T] = Validator[T](name, msg) { t =>
    func(t).length >= len
  }

  def maxLength[T](name: String, msg: NodeSeq)(len: Int, func: (T) => String): Validator[T] = Validator[T](name, msg) { t =>
    func(t).length <= len
  }
}

object ValError {
  implicit class flashValErrors(errors: List[ValError]) {
    def flash(): Unit = errors.foreach(_.flash())
  }

  implicit class flashValError(error: ValError) {
    def flash(): Unit = {
      error match {
        case NamedError(name, msg) => S.error(name, msg)
        case AnonError(msg) => S.error(msg)
      }
    }
  }
}
