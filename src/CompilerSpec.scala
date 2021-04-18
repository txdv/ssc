package lt.vu.mif.bentkus.bachelor.compiler

import org.scalatest._
import flatspec._
import matchers._

import lt.vu.mif.bentkus.bachelor.compiler.parser.scala.Expression.{
  DefObject
}

import collection.JavaConverters._

class CompilerSpec extends AnyFlatSpec with should.Matchers {
  def compileAndRun(code: String): String = {
    val codeBytes = code.getBytes
    val statements = ScalaCompiler.parseBytes(codeBytes)
    val defObject = statements.find(_.isInstanceOf[DefObject]).get.asInstanceOf[DefObject]
    val name = defObject.name
    val objDef = ScalaCompiler.convert(defObject)
    val m = new classfile.higher.Materializer
    val classFile = m.bytes(objDef)
    val jclass = loadBytes(name, classFile.getBytes)
    runMethod(jclass, "main", Array[String]())
  }

  def loadBytes(name: String, bytes: Array[Byte]): Class[_] = {
    val cl = classOf[CompilerSpec].getClassLoader
    val urls = Array[java.net.URL]()
    val extra = new java.util.HashMap[String, Array[Byte]]()
    extra.put(name, bytes)
    val bcl = new ByteClassLoader(urls, cl, extra)
    bcl.findClass(name)
  }

  def runMethod(jclass: java.lang.Class[_], name: String, arg: Any): String = {

    val method = jclass.getMethods.find(_.getName == "main").get

    val baos = new java.io.ByteArrayOutputStream
    val oldOut = System.out
    System.setOut(new java.io.PrintStream(baos));

    method.invoke(method, Array[String]())

    System.setOut(oldOut)

    baos.toString
  }

  "Compiler" should "println string" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println("Hello World!")
        }
      }
    """ } should be ("Hello World!\n")
  }

  "Compiler" should "concat string" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println("Hello" + " " + "World!")
        }
      }
    """ } should be ("Hello World!\n")
  }

  "Compiler" should "add together numbers" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println(1 + 2)
        }
      }
    """ } should be ("3\n")
  }

  "Compiler" should "add grouped numbers" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println(2 * (3 + 4))
        }
      }
    """ } should be ("14\n")
  }
}
