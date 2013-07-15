package hexico.foostack.parsers

import scala.util.parsing.combinator.JavaTokenParsers

trait LangParser extends JavaTokenParsers with NumericLiteralParsers {
  sealed trait AST
  case class Sexpr(items: List[AST]) extends AST
  sealed trait Atom extends AST
  case class Value(value: Any) extends Atom
  case class Identifier(id: String) extends Atom

  override def skipWhitespace = false

  def identifier: Parser[AST] = """[^\s()0-9'"][^\s()'"]*""".r ^^ Identifier
  def atom: Parser[AST] = (stringLiteral | doubleLiteral | longLiteral) ^^ Value | identifier
  def expr: Parser[AST] = atom | sexpr
  def sexpr: Parser[AST] = "(" ~> repsep(expr, whiteSpace) <~ ")" ^^ Sexpr
  def lang: Parser[List[AST]] = opt(whiteSpace) ~> repsep(sexpr, whiteSpace) <~ opt(whiteSpace)
  override def stringLiteral: Parser[String] = super.stringLiteral ^^ (s => s.substring(1, s.length - 1))
}