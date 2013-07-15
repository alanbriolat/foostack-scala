package hexico.foostack.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import java.lang.{ Long => JLong, Double => JDouble }

trait NumericLiteralParsers { self: JavaTokenParsers =>
  def decLongLiteral: Parser[Long] = wholeNumber ^^ (JLong.parseLong(_))
  def hexLongLiteral: Parser[Long] = "0x" ~> "[0-9a-fA-F]+".r ^^ (JLong.parseLong(_, 16))
  def octLongLiteral: Parser[Long] = "0" ~> "[0-7]+".r ^^ (JLong.parseLong(_, 8))
  def binLongLiteral: Parser[Long] = "0b" ~> "[01]+".r ^^ (JLong.parseLong(_, 2))
  def longLiteral: Parser[Long] = hexLongLiteral | binLongLiteral | octLongLiteral | decLongLiteral

  private def unambiguousFloat(s: String): Boolean = s.contains(".") || s.toLowerCase.contains("e")
  def doubleLiteral: Parser[Double] = floatingPointNumber.withFilter(unambiguousFloat) ^^ (JDouble.parseDouble(_))
}