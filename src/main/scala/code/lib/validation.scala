package code.lib

import java.util.regex.Pattern

import scala.xml.NodeSeq

import net.liftweb.http._
import net.liftweb.common._

package object validation {
  trait ValError

  case class AnonError(msg: NodeSeq) extends ValError
  case class NamedError(name: String, msg: NodeSeq) extends ValError

  object ValError {
    def flash(e: ValError) = e match {
      case AnonError(msg) => S.error(msg)
      case NamedError(name, msg) => S.error(name, msg)
    }
  }

  trait Validatable[T] extends Loggable {
    type ValFunc = (T) => List[ValError]

    def apply(obj: T): List[ValError] = validate(obj)

    def validators: List[ValFunc]

    def validate(obj: T): List[ValError] = validators.map(f => f(obj)).flatten

    def validate2[A](obj: T)(errorFunc: (ValError) => Unit)(success: => A): Option[A] = {
      validate(obj) match {
        case Nil => Some(success)
        case e => {
          e.map(errorFunc)
          None
        }
      }
    }

    def valGeneric(f: (T) => Boolean, name: String, msg: NodeSeq): ValFunc =
    { obj: T =>
      f(obj) match {
        case true => Nil
        case false => List(NamedError(name, msg))
      }
    }

    def valRequired(f: (T) => String, name: String, msg: NodeSeq) =
      valMinLength(f, 1, name, msg)

    def valLength(f: (T) => String, tester: (Int) => Boolean, name: String, msg: NodeSeq) =
      valGeneric((obj) => tester(f(obj).length), name, msg)

    def valMinLength(f: (T) => String, minLength: Int, name: String, msg: NodeSeq) =
      valLength(f, _ >= minLength, name, msg)

    def valMaxLength(f: (T) => String, maxLength: Int, name: String, msg: NodeSeq) =
      valLength(f, _ <= maxLength, name, msg)

    def valEmail(f: (T) => String, name: String, msg: NodeSeq) = {
      val pattern = Pattern.compile("^[a-z0-9._%\\-+]+@(?:[a-z0-9\\-]+\\.)+[a-z]{2,4}$")
      valGeneric((obj) => pattern.matcher(f(obj)).matches, name, msg)
    }
  }
}
