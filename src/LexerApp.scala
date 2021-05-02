package lt.vu.mif.bentkus.bachelor.compiler.lexer

import lt.vu.mif.bentkus.bachelor.compiler.span.Span

import java.io.File
import java.nio.file.Files

object LexerApp extends App {
  val file = args.headOption.getOrElse {
    println("Please provide file to parse")
    System.exit(1)
    ???
  }

  val path = new File(file).toPath
  val bytes = Files.readAllBytes(path)

  val lexems = Lexer.lexAll(Span(bytes))
  println(lexems)
  lexems.foreach { byte =>
    print(byte.getString)
  }
}
