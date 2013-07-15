package hexico.foostack.compiler

import hexico.foostack.parsers.LangParser

object Compiler extends LangParser {
  def main(args: Array[String]) {
    println(parseAll(doubleLiteral, "-8e-3"))
    println(parseAll(lang,
      """
        |(hello "world" 123 -8e-3)
      """.stripMargin))
  }
}