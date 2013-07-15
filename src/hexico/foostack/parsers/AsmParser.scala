package hexico.foostack.parsers

import scala.util.parsing.combinator.RegexParsers
import java.lang.{ Long => JLong }
import hexico.foostack.cpu.Asm

trait AsmParser extends RegexParsers {
  sealed trait Value
  case class Literal(value: Long) extends Value
  case class LabelRef(label: String) extends Value
  case class SymbolRef(label: String) extends Value

  sealed trait Operand
  case class Immediate(value: Value) extends Operand
  case class DataRegister(r: Asm.DataRegister.Value) extends Operand
  case class AddrRegister(r: Asm.AddrRegister.Value) extends Operand
  case class Indirect(r: Asm.AddrRegister.Value) extends Operand
  case class IndirectOffset(r: Asm.AddrRegister.Value, offset: Value) extends Operand

  sealed trait AST
  case class Instruction(mnemonic: String, operands: List[Operand]) extends AST
  case class Symbol(label: String) extends AST
  case class Label(label: String) extends AST
  case class Directive(name: String, args: List[String]) extends AST

  override def skipWhitespace = true
  // Redefine whitespace to not skip newlines
  override val whiteSpace = """[ \t]+""".r

  val commentChar = ';'

  // A generic pattern for a sequence of same-typed parsers separated by another parser
  def seq[T](sep: Parser[Any], parsers: List[Parser[T]]): Parser[List[T]] = {
    parsers.head ~ (parsers.tail match {
      case List() => success(List())
      case tail => sep ~> seq(sep, tail)
    }) ^^ mkList
  }

  // Define newline as a token we can match
  def newLine: Parser[String] = """\r\n|\r|\n""".r
  // General-purpose token for anything that isn't whitespace or the start of a comment
  def text: Parser[String] = """[^\s"""+commentChar+"""]+""".r
  // Identifier: alphanumeric + "_", but can't start with a number
  def identifier: Parser[String] = "[_a-zA-Z][_a-zA-Z0-9]*".r

  // Ref: a symbol or label reference (labels start with _)
  def ref: Parser[Value] = opt("_") ~ identifier ^^ {
    case Some("_") ~ label => LabelRef(label)
    case None ~ label => SymbolRef(label)
  }
  // Literal: representation of a numeric value
  def decLiteral: Parser[Long] = "0|[+-]?[1-9][0-9]*".r ^^ (JLong.parseLong(_, 10))
  def hexLiteral: Parser[Long] = "0x" ~> "[0-9a-fA-F]+".r ^^ (JLong.parseLong(_, 16))
  def octLiteral: Parser[Long] = "0" ~> "[0-7]+".r ^^ (JLong.parseLong(_, 8))
  def binLiteral: Parser[Long] = "0b" ~> "[01]+".r ^^ (JLong.parseLong(_, 2))
  def literal: Parser[Value] = (hexLiteral | binLiteral | octLiteral | decLiteral) ^^ (Literal(_))
  // Immediate: a reference or literal value
  def immediate: Parser[Value] = ref | literal
  // Register names
  def dataRegister: Parser[Asm.DataRegister.Value] = "[dD][0-7]".r ^^ (_.toUpperCase) ^^ Asm.DataRegister.withName
  def addrRegister: Parser[Asm.AddrRegister.Value] = "[aA][0-7]".r ^^ (_.toUpperCase) ^^ Asm.AddrRegister.withName
  // Register-based operands
  def dataDirect: Parser[Operand] = dataRegister ^^ DataRegister
  def addrDirect: Parser[Operand] = addrRegister ^^ AddrRegister
  def addrIndirect: Parser[Operand] = "(" ~> (opt(immediate <~ ",") ~ addrRegister) <~ ")" ^^ {
    case None ~ addr => Indirect(addr)
    case Some(offset) ~ addr => IndirectOffset(addr, offset)
  }
  // Operand: any valid operand
  def operand: Parser[Operand] = dataDirect | addrDirect | addrIndirect | immediate ^^ Immediate

  // Label: "_foo:", a name for a code location, valid between symbols
  def label: Parser[AST] = "_" ~> identifier <~ ":" ^^ Label
  // Symbol: "foo:", a name for a code location, valid globally
  def symbol: Parser[AST] = identifier <~ ":" ^^ Symbol
  // Instruction: mnemonic followed by appropriate operands
  def instruction: Parser[Instruction] = identifier ~ repsep(operand, ",") ^^ { case mnemonic ~ args => Instruction(mnemonic, args) }
  // Directive: .identifier [arg ...]
  def directive: Parser[AST] = "." ~> identifier ~ rep(text) ^^ { case id ~ args => Directive(id, args) }
  // Statement: any of the possible kinds of statements
  def statement: Parser[AST] = label | symbol | directive | instruction
  // Comment: ; to the end of the line
  def comment: Parser[String] = commentChar ~> ".+".r
  // Line: statement followed by comment, both optional
  def line: Parser[Any] = opt(statement) <~ opt(comment)
  // Assembly file: a sequence of lines
  def asm: Parser[Any] = repsep(line, newLine)
}