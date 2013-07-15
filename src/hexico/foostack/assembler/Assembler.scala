package hexico.foostack.assembler

import hexico.foostack.parsers.AsmParser

object Assembler extends AsmParser {
  def main(args: Array[String]) {
    println(parseAll(asm,
      """; hello world
        |a_symbol:
        |   this a7, +212
        |_a_label:
        |   that (-8,a7)
        |
      """.stripMargin))
  }
}


/*
sealed trait Operand
case class Register(r: Asm.Register.Value) extends Operand
case class HalfRegister(r: Asm.HalfRegister.Value) extends Operand
case class Immediate(i: Byte) extends Operand
case class LabelRef(l: String, high: Boolean) extends Operand
case class SymbolRef(s: String, high: Boolean) extends Operand

sealed trait ASTNode
case class Instruction(op: String, operands: List[Operand]) extends ASTNode
case class Directive(directive: String, args: List[String]) extends ASTNode
case class Symbol(symbol: String) extends ASTNode
case class Label(label: String) extends ASTNode


trait AsmParser extends RegexParsers {
  override def skipWhitespace = true
  // Redefine whitespace to not skip newlines
  override val whiteSpace = """[ \t]+""".r

  // A generic pattern for a sequence of same-typed parsers separated by another parser
  def seq[T](sep: Parser[Any], parsers: List[Parser[T]]): Parser[List[T]] = {
    parsers.head ~ (parsers.tail match {
      case List() => success(List())
      case tail => sep ~> seq(sep, tail)
    }) ^^ mkList
  }

  // Define newline (or a group of newlines) as a token we can match
  def newLine: Parser[String] = """\r\n|\r|\n""".r
  // General-purpose token for anything that isn't whitespace or the start of a comment
  def text: Parser[String] = """[^\s;]+""".r
  // Identifier: alphanumeric + "_", but can't start with a number
  def identifier: Parser[String] = "[_a-zA-Z][_a-zA-Z0-9]*".r
  // Word: case-insensitive (forced uppercase) string of letters
  def word: Parser[String] = "[a-zA-Z]+".r ^^ (_.toUpperCase)

  // Mnemonic: a word which is a valid op name
  def mnemonic: Parser[String] = word.withFilter(Asm.ops.contains(_))
  // Register: a word which is a valid register name
  def register: Parser[Operand] = word ^^ Asm.Register.withName ^^ Register
  // Half register: a word which is a valid half-register name
  def halfRegister: Parser[Operand] = word ^^ Asm.HalfRegister.withName ^^ HalfRegister
  def decLiteral: Parser[Byte] = "0|[+-]?[1-9][0-9]+".r ^^ (Integer.parseInt(_, 10).toByte)
  def hexLiteral: Parser[Byte] = "0x" ~> "[0-9a-fA-F]+".r ^^ (Integer.parseInt(_, 16).toByte)
  def octLiteral: Parser[Byte] = "0" ~> "[0-7]+".r ^^ (Integer.parseInt(_, 8).toByte)
  def binLiteral: Parser[Byte] = "0b" ~> "[01]+".r ^^ (Integer.parseInt(_, 2).toByte)
  // Byte: a numeric literal
  def byte: Parser[Immediate] = (hexLiteral | binLiteral | octLiteral | decLiteral) ^^ (Immediate(_ ))
  // Ref: a label/symbol reference, consisting of a high/low (</>) byte indicator and the
  // label identifier (labels start with _, symbols do not)
  def ref: Parser[Operand] = ("<" | ">") ~ opt("_") ~ identifier ^^ {
    case highlow ~ Some("_") ~ id => LabelRef(id, highlow == "<")
    case highlow ~ None ~ id => SymbolRef(id, highlow == "<")
  }
  // Immediate: a value literal, either directly or a label location
  def immediate: Parser[Operand] = byte | ref
  // Instruction: mnemonic followed by appropriate operands
  def instruction: Parser[Instruction] = mnemonic >> withOperands
  // Operands: a op-dependent sequence of operands
  def withOperands(op: String): Parser[Instruction] = {
    Asm.ops(op) match {
      case Asm.Op_ => success(Instruction(op, List()))
      case Asm.OpR => register ^^ (List(_)) ^^ (Instruction(op, _))
      case Asm.OpI => immediate ^^ (List(_)) ^^ (Instruction(op, _))
      case Asm.OpRR => seq(",", List(register, register)) ^^ (Instruction(op, _))
      case Asm.OpRI => seq(",", List(register, immediate)) ^^ (Instruction(op, _))
      case Asm.OpHH => seq(",", List(halfRegister, halfRegister)) ^^ (Instruction(op, _))
      case Asm.OpHI => seq(",", List(halfRegister, immediate)) ^^ (Instruction(op, _))
    }
  }

  // Directive: .identifier [arg ...]
  def directive: Parser[ASTNode] = "." ~> identifier ~ rep(text) ^^ { case id ~ args => Directive(id, args) }
  // Statement: any of the possible kinds of statements
  def statement: Parser[ASTNode] = directive | instruction
  // Comment: ; to the end of the line
  def comment: Parser[String] = ";" ~> ".+".r
  // Line: statement followed by comment, both optional
  def line: Parser[Any] = opt(statement) <~ opt(comment)
  // Assembly file: a sequence of lines
  def assembler: Parser[Any] = repsep(line, newLine)
}

object Assembler extends AsmParser {
  def main(args: Array[String]) {
    println()
    println(parseAll(assembler,
      """; Test comments
        |; I am a comment
        |    ; so am i
        |; Test line comments
        |.directive ;with comment
        |
        |HALT
        |; Test immediate values
        |LDBI AH, 0x20
        |LDBI AH, 0b0011
        |LDBI AH, 070
        |LDBI AH, 52
        |
        |; Test label references
        |LDBI BH, <_foo
        |LDBI BL, >_foo
        |; Test symbol references
        |LDBI CH, <something
        |; Test case-insensitivity
        |ldbi cl, >something
      """.stripMargin))
  }
}
*/
