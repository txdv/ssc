package ssc.parser.skylark

import ssc.lexer.Lexer
import ssc.parser.Parser
import ssc.span.Span

object SkylarkApp extends App {
  import java.io.File
  import java.nio.file.Files

  val content = Files.readAllBytes(new File(args(0)).toPath)

  val s = Parser.parseTokens(Skylark.expressions, Lexer.lexAll(Span(content)).toList)
  println(s)
}
