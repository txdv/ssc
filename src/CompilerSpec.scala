package ssc

import java.lang.reflect.InvocationTargetException

import org.scalatest._
import flatspec._
import matchers._
import ssc.parser.scala.AST.ObjectDecl

import collection.JavaConverters._

class CompilerSpec extends AnyFlatSpec with should.Matchers {
  def compileAndRun(code: String): String = {
    val codeBytes = code.getBytes
    val statements = ScalaCompiler.parseBytes(codeBytes)
    val defObject = statements.find(_.isInstanceOf[ObjectDecl]).get.asInstanceOf[ObjectDecl]
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

    try {
      method.invoke(method, Array[String]())
    } catch {
      case ex: InvocationTargetException =>
        throw ex.getTargetException
    }

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

  "Compiler" should "add together multiple numbers" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println(1 + 2 + 3 + 4 + 5)
        }
      }
    """ } should be ("15\n")
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

  "Compiler" should "true" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println(true)
        }
      }
    """ } should be ("true\n")
  }

  "Compiler" should "false" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println(false)
        }
      }
    """ } should be ("false\n")
  }

  "Compiler" should "concat string and bool" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println("Hello " + false)
        }
      }
    """ } should be ("Hello false\n")
  }

  "Compiler" should "simple if true" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println(if (true) 1 else 0)
        }
      }
    """ } should be ("1\n")
  }

  "Compiler" should "simple if false" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println(if (false) 1 else 0)
        }
      }
    """ } should be ("0\n")
  }

  "Compiler" should "==" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println(Math.addExact(1, 2) == 3)
        }
      }
    """ } should be ("true\n")
  }

  "Compiler" should "generate NotImplementedError for ???" in {
    assertThrows[NotImplementedError] {
      compileAndRun {
        """
      object MainApp {
        def main(args: Array[String]): Unit = {
          ???
        }
      }
    """
      }
    }
  }
/*
  "Compiler" should "stack depth check" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println(Math.addExact(1, 2) == Math.addExact(2, 3) + Math.addExact(4, 5))
        }
      }
    """ } should be ("false\n")
  }
*/

  "Compiler" should "method with two lines" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          println(1)
          println(2)
        }
      }
    """ } should be ("1\n2\n")

  }

  "Compiler" should "method with variable" in {
    compileAndRun { """
      object MainApp {
        def main(args: Array[String]): Unit = {
          val a: Int = 1
          println(a)
        }
      }
    """ } should be ("1\n2\n")

  }
}
