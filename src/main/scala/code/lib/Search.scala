package code.lib

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

import Slick._

sealed trait TextQuery {
  def query: String
}

case class AndQuery(left: TextQuery, right: TextQuery) extends TextQuery {
  override def query = s"${left.query} & ${right.query}"
}

case class OrQuery(left: TextQuery, right: TextQuery) extends TextQuery {
  override def query = s"${left.query} | ${right.query}"
}

case class NotQuery(q: TextQuery) extends TextQuery {
  override def query = s"!${q.query}"
}

case class TermQuery(term: String) extends TextQuery {
  override def query = s"$term:*"
}

object TextQuery extends RegexParsers {
  override def skipWhitespace = true

  def and: Parser[AndQuery] = term ~ ("and" ~> query) ^^ { case (l ~ r) => AndQuery(l, r) }
  def or: Parser[OrQuery] = term ~ (("or" ~> query) | query) ^^ { case (l ~ r) => OrQuery(l, r) }
  def not: Parser[NotQuery] = "not" ~> query ^^ { q => NotQuery(q) }
  def term: Parser[TermQuery] = """[^\s:|&!]+""".r ^^ { t => TermQuery(t) }

  def query: Parser[TextQuery] = and | or | not | term

  def apply(s: String): Option[TextQuery] = {
    parseAll(query, s.filterNot(":|&!'" contains _).toLowerCase) match {
      case Success(result, _) => Some(result)
      case _ => None
    }
  }
}
