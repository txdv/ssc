package ssc.jar

import ssc.Hex
import java.util.zip.{ZipFile, ZipEntry}
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
      handleEntry(zip, entry)
    }
  }

  def stream(filepath: String): LazyList[File] = {
    val zip = new ZipFile(filepath)
    zip.entries.asScala.to(LazyList).flatMap { entry =>
      handleEntry(zip, entry)
    }

  }

  private def handleEntry(zip: ZipFile, entry: ZipEntry): Option[File] = {
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

object MainApp {

  def removeSuffix(string: String, suffix: String): String = {
    if (string.endsWith(suffix)) {
      string.substring(0, string.length - suffix.length)
    } else {
      string
    }
  }
  

  //val filePool = Executors.newFixedThreadPool(48)
  def main(args: Array[String]): Unit = {
    //val tp = Executors.newFixedThreadPool(24)
    //implicit val ec = ExecutionContext.fromExecutor(tp)

    handle(args.head)

    //Await.result(Future.sequence(futures), 60.seconds)
    //tp.shutdown()
    //filePool.shutdown()
  }


  def handle(path: String): Unit = {
    val files = Jar.unzip(path)

    val classFiles = files.filter(_.name.endsWith(".class"))

    val classFileNames = classFiles.map(classFile => removeSuffix(classFile.name, ".class"))
    //classFileNames.foreach(println)

    val t = classFiles.filter(_.name.endsWith("Array.class"))

    t.foreach { file =>
      val f = ClassFile.parse(file.content)
      implicit val classFile = f
      val thisClass = f.constants(f.thisClass - 1).asInstanceOf[Constant.Class].stringName
      //val thisClass = f.constants(0)
      //println(s"$thisClass method count: ${f.methods.size}")
      f.constants.zipWithIndex.foreach { case (constant, i) =>
        val idx = i + 1
        println(s"#$idx: $constant")
      }
      f.methods.foreach { method =>
        //println(s"  ${method.getName} ${method.getDescriptor}")
      }

      f.attributes.foreach { attribute =>
        val name = attribute.getName
        println(s"$name 0x${Hex.encode(attribute.info)}")
      }
    }
  }

  def handle2(path: String): Unit = {
    val files = Jar.stream(path)

    files.foreach { file =>
      val f = ClassFile.parse(file.content)
      implicit val classFile = f
      val thisClass = f.constants(f.thisClass - 1).asInstanceOf[Constant.Class].stringName
      //val thisClass = f.constants(0)
      //println(s"$thisClass method count: ${f.methods.size}")
      f.methods.foreach { method =>
        //println(s"  ${method.getName} ${method.getDescriptor}")
      }
      println(f.attributes)
    }

  }
}
