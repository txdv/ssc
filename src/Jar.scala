package ssc.jar

import org.json4s.scalap.scalasig._
import ssc.parser.scala.AST
import ssc.parser.scala.AST.{ClassDecl, MethodDecl, SimpleType}
//{MethodSymbol, NullaryMethodType, ScalaSig}
import ssc.Hex
import ssc.misc.PrettyPrint

import java.util.zip.{ZipEntry, ZipFile}
import java.util.concurrent.Executors
import scala.collection.JavaConverters._
import ssc.classfile.{ClassAttribute, ClassFile, Constant}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

object Jar {
  case class File(name: String, content: Array[Byte])

  def unzip(filepath: String): Seq[File] = {
    val zip = new ZipFile(filepath)
    try {
      zip.entries.asScala.toSeq.flatMap { entry =>
        handleEntry(zip, entry)
      }
    } finally {
      zip.close
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

    args.toSeq match {
      case Seq(jarFile, classFile) =>
        handleJar(jarFile, classFile)
      case Seq(jarFile) =>
        //handleOld(jarFile)
        handleJar(jarFile)
    }

    //Await.result(Future.sequence(futures), 60.seconds)
    //tp.shutdown()
    //filePool.shutdown()
  }


  def handleJar(path: String, file: String): Unit = {
    val files = Jar.unzip(path)

    files.filter(_.name == file).map { file =>
      implicit val f = ClassFile.parse(file.content)

      findVRA
    }
  }

  def handleJar(path: String): Unit = {
    val scalaSignatures = Jar.unzip(path).filter(_.name.endsWith(".class")).flatMap { file =>
      //println()
      //println(file.name)
      implicit val f = ClassFile.parse(file.content)
      findVRA
    }


    scalaSignatures.foreach { signatures =>
      val entries = (0 until signatures.table.length)
        .map(signatures.parseEntry)

      val methods = findMethods(signatures, entries)

      signatures.topLevelObjects
      val obj = entries.collectFirst {
        case o: ObjectSymbol =>
          o
      }

      val classDecl = entries.collectFirst {
        case k: ClassSymbol =>
          ClassDecl(k.name, methods)
      }

      classDecl.foreach(PrettyPrint.pformat)


    }
  }

  private def findMethods(signatures: ScalaSig, entries: Seq[Any]): Seq[MethodDecl] = {
    signatures.topLevelObjects
    entries.collect {
      case methodSymbol: MethodSymbol =>
        val info = signatures.parseEntry(methodSymbol.symbolInfo.info)
        info match {
          case NullaryMethodType(t) =>
            Some(MethodDecl(methodSymbol.name, convert(t)))
          case MethodType(t, symbols) =>
            val args = symbols.map {
              case methodSymbol: MethodSymbol =>
                val t = signatures.parseEntry(methodSymbol.symbolInfo.info)
                AST.MethodDeclArgument(methodSymbol.name, convert(t.asInstanceOf[Type]))
            }
            Some(MethodDecl(methodSymbol.name, convert(t), args))
          case _ =>
            Option.empty
        }
    }
    .flatten
  }


  private def convert(t: Type): AST.ScalaType = t match {
    case t@TypeRefType(ThisType(_), symbol, typeArgs) =>
      //println(t)
      SimpleType(symbol.name)
    case o =>
      println(o)
      ???
  }

  import org.json4s.scalap.scalasig.ScalaSig

  private def findVRA(implicit classFile: ClassFile): Seq[ScalaSig] = {
    /*classFile.attributes.find { _.getName == "RuntimeVisibleAnnotations" }.map { rva =>
      ClassAttribute(rva)
    }*/
    val attributes = classFile.attributes.map(a => ClassAttribute(a))

    attributes.flatMap {
      case ClassAttribute.RuntimeVisibileAnnotations(annotations) =>
        import ClassAttribute.RuntimeVisibileAnnotation
        import ClassAttribute.RuntimeVisibileAnnotation.Str

        annotations.flatMap {
          case RuntimeVisibileAnnotation("Lscala/reflect/ScalaSignature;", Seq(Str("bytes", scalaSig))) =>
            //import org.json4s.scalap.scalasig.{ByteCode, ScalaSigAttributeParsers, ScalaSigParser, ClassFileParser}
            import org.json4s.scalap.scalasig._
            import org.json4s.scalap.ByteCodecs
            val length = ByteCodecs.decode(scalaSig)
            val result = ScalaSigAttributeParsers.parse(ByteCode(scalaSig.take(length)))
            Option(result)

            /*
            (0 until result.table.size).map { i =>
              result.parseEntry(i) match {
                case classSymbol: ClassSymbol =>
                  println(s"$i: $classSymbol")
                case methodSymbol: MethodSymbol =>
                  println(s"$i: $methodSymbol")
                case o =>
                  println(s"$i: $o")
              }
            }*/


            //println(result.getClass)
            //println(result)
        }
        /*
        annotations.find(_.annotationType == "Lscala/reflect/ScalaSignature;").foreach {
          case Seq(Str("bytes", value)) =>
            println(value)

        }*/
      case _ =>
        None
    }
  }

  /*
  classFile.attributes.map(a => ClassAttribute(a)).foreach { ca =>
  ca match {
  case ClassAttribute.Info(name, content) =>
    println(name + " " + Hex.format(content))
  case ClassAttribute.RuntimeVisibileAnnotations(annotations) =>
    annotations.foreach { annotation =>
      println(annotation.annotationType)
      annotation.fields.foreach {
        case ClassAttribute.RuntimeVisibileAnnotation.Str(name, content) =>
          println(Hex.format(content))
        case _ =>
      }

    }
      case o =>
        //println(o)
    }
  }
  */
  def handleOld(path: String): Unit = {
    val files = Jar.unzip(path)

    val classFiles = files.filter(_.name.endsWith(".class"))
    val classFileNames = classFiles.map(classFile => removeSuffix(classFile.name, ".class"))
    //classFileNames.foreach(println)

    val t = classFiles.filter(_.name.endsWith("Array.class"))

    import org.json4s.scalap.scalasig.{ByteCode, ScalaSigAttributeParsers, ScalaSigParser, ClassFileParser}
    import org.json4s.scalap.ByteCodecs

    t.foreach { file =>
      println {
        val classFile = ClassFileParser.parse(ByteCode(file.content))
        ScalaSigParser.scalaSigFromAnnotation(classFile)
      }

      val f = ClassFile.parse(file.content)
      implicit val classFile = f
      val thisClass = f.constants(f.thisClass - 1).asInstanceOf[Constant.Class].stringName
      //val thisClass = f.constants(0)
      //println(s"$thisClass method count: ${f.methods.size}")

      f.constants.zipWithIndex.foreach { case (constant, i) =>
        val idx = i + 1
        println(s"#$idx: $constant")
      }

      println("HERE:")
      val scalaSig = f.constants(8).asInstanceOf[Constant.Utf8].bytes
      println(scalaSig.length)

      val length = ByteCodecs.decode(scalaSig)
      val result = ScalaSigAttributeParsers.parse(ByteCode(scalaSig.take(length)))
      println(result)

      //val parser = ScalaSigAttributeParsers.parse(ByteCode(scalaSig))

      //println(parser)


      /*f.methods.foreach { method =>
        println(s"  ${method.getName} ${method.getDescriptor}")
      }*/

      f.attributes.foreach { attribute =>
        val name = attribute.getName
        println(s"$name ${attribute.info.length} 0x${Hex.encode(attribute.info)}")
        println(attribute)
        println(attribute.info.length)
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
