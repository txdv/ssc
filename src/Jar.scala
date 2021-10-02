package ssc.jar

import java.util.zip.ZipFile
import java.util.concurrent.Executors
import scala.collection.JavaConverters._
import ssc.classfile.{ClassFile, Constant}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

object Jar {
  case class File(name: String, content: Array[Byte])

  def unzip(filepath: String): Seq[File] = {
    val zip = new ZipFile(filepath)
    zip.entries.asScala.toSeq.flatMap { entry =>
      if (entry.isDirectory) {
        Option.empty[File]

      } else {
        val name = entry.getName
        val fileStream = zip.getInputStream(entry)
        val bytes = fileStream.readAllBytes()
        fileStream.close()
        Some(File(entry.getName, bytes))
      }
    }
  }
}

object MainApp {

  def removeSuffix(string: String, suffix: String): String = {
    if (string.endsWith(suffix)) {
      string.substring(0, string.length - suffix.length)
    } else {
      string
    }
  }

  def main(args: Array[String]): Unit = {
    val tp = Executors.newFixedThreadPool(24)
    implicit val ec = ExecutionContext.fromExecutor(tp)

    var i = 1
    val futures = (1 to 100).map { _ =>
      Future {
        val j = i
        i += 1
        println(s"Start $j")
        handle(args.head)
        println(s"End $j")
        j
      }
    }

    Await.result(Future.sequence(futures), 60.seconds)
    tp.shutdown()
  }


  def handle(path: String): Unit = {
    val files = Jar.unzip(path)

    val classFiles = files.filter(_.name.endsWith(".class"))

    val classFileNames = classFiles.map(classFile => removeSuffix(classFile.name, ".class"))
    //classFileNames.foreach(println)

    classFiles.foreach { file =>
      val f = ClassFile.parse(file.content)
      implicit val classFile = f
      val thisClass = f.constants(f.thisClass - 1).asInstanceOf[Constant.Class].stringName
      //val thisClass = f.constants(0)
      //println(s"$thisClass method count: ${f.methods.size}")
      f.methods.foreach { method =>
        //println(s"  ${method.getName} ${method.getDescriptor}")
      }
    }

  }
}
