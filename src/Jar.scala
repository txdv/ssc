package ssc.jar

import org.json4s.scalap.scalasig._
import ssc.parser.scala.AST
import ssc.parser.scala.AST.{ClassDecl, MethodDecl, Repeated, SimpleType}
import ssc.misc.PrettyPrint

import java.util.zip.{ZipEntry, ZipFile}
import scala.collection.JavaConverters._
import ssc.classfile.{ClassAttribute, ClassFile}

object Jar {
  case class File(name: String, content: Array[Byte])

  def unzip(filepath: String, filter: ZipEntry => Boolean): Iterator[File] = {
    val zip = new ZipFile(filepath)
    try {
      zip.entries.asScala.filter(filter).flatMap { entry =>
        handleEntry(zip, entry)
      }
    } finally {
      //zip.close
    }
  }

  def stream(filepath: String): Iterator[File] = {
    val zip = new ZipFile(filepath)
    zip.entries.asScala.flatMap { entry =>
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
      Some(File(name, bytes))
    }
  }
}


object MainApp {
  def main(args: Array[String]): Unit = {

    args.toSeq match {
      case Seq(jarFile, classFile) =>
        handleJar(jarFile, Option(classFile))
      case Seq(jarFile) =>
        handleJar(jarFile, Option.empty)
    }
  }

  def handleJar(path: String, findFile: Option[String]): Unit = {
    ScalaSignature.load(path, findFile).foreach { decl =>
      PrettyPrint.pformat(decl)
    }
  }

}

object ScalaSignature {

  def removeSuffix(string: String, suffix: String): String = {
    if (string.endsWith(suffix)) {
      string.substring(0, string.length - suffix.length)
    } else {
      string
    }
  }


  def load(path: String, findFile: Option[String] = Option.empty): Iterator[AST.Decl] = {

    val scalaSignatures = Jar
      .unzip(path, entry => findFile.forall(ff => entry.getName.endsWith(ff)))
      .filter(_.name.endsWith(".class"))
      .flatMap { file =>
        val classFile = ClassFile.parse(file.content)
        rescue {
          findVRA(classFile).map(sig => (sig, file.name))
        }
      }.flatten

    scalaSignatures.flatMap { case (signatures, name) =>
      findDecls(signatures)
    }

  }

  private def rescue[T](f: => T): Option[T] = {
    try Option {
      f
    } catch {
      case throwable: Throwable =>
        throwable.printStackTrace()
        Option.empty
    }
  }

  private def findDecls(signatures: ScalaSig): Seq[AST.Decl] = {
    val entries = (0 until signatures.table.length)
      .map(signatures.parseEntry)

    /*
    debug(entries)
    println(s"file: ${name}")
    println(signatures)
    */

    val methods = findMethods(signatures, entries)

    val objectDecls = entries.collect {
      case o: ObjectSymbol =>
        getName(o)
    }

    val classDecls = entries.collect {
      case k: ClassSymbol =>
        val klassMethods = methods.filter(_._1 == k.index).map(_._2)
        val name = getName(k)
        if (objectDecls.contains(name)) AST.ObjectDecl(name, klassMethods)
        else ClassDecl(name, klassMethods)
    }

    classDecls
  }

  private def debug(entries: Seq[Any]): Unit = {
    entries.filter(_ != null).zipWithIndex.groupBy(_._1.getClass).foreach { case (klass, entries) =>
      println(klass)
      entries.foreach { case (entry, i) =>
        println(s"\t $i: $entry")
      }
    }
  }

  private def getName(symbol: Symbol): String = symbol match {
    case methodSymbol: MethodSymbol =>
      methodSymbol.toString
    case classSymbol: ClassSymbol if classSymbol.name == "<refinement>" =>
      // TODO: handle refinement?
      "<refinement>"
    case classSymbol: ClassSymbol =>
      getName(classSymbol.symbolInfo.owner) + "." + classSymbol.name
    case objectSymbol: ObjectSymbol =>
      getName(objectSymbol.symbolInfo.owner) + "." + objectSymbol.name
    case externalSymbol: ExternalSymbol =>
      externalSymbol.toString
    case _ =>
      println(symbol)
      ???
  }

  private def findMethods(signatures: ScalaSig, entries: Seq[Any]): Seq[(Int, MethodDecl)] = {
    signatures.topLevelObjects
    entries.collect {
      case methodSymbol: MethodSymbol =>
        val ownerIndex = methodSymbol.symbolInfo.owner.asInstanceOf[SymbolInfoSymbol].index

        val info = signatures.parseEntry(methodSymbol.symbolInfo.info)
        info match {
          case NullaryMethodType(t) =>
            val methodDecl = MethodDecl(methodSymbol.name, convert(t))
            Some((ownerIndex, methodDecl))
          case MethodType(t, symbols) =>
            val args = symbols.map {
              case methodSymbol: MethodSymbol =>
                val t = signatures.parseEntry(methodSymbol.symbolInfo.info)
                AST.MethodDeclArgument(methodSymbol.name, convert(t.asInstanceOf[Type]))
            }
            val methodDecl = MethodDecl(methodSymbol.name, convert(t), args)
            Some((ownerIndex, methodDecl))
          case _ =>
            Option.empty
        }
    }
    .flatten
  }


  private def convert(t: Type): AST.ScalaType = t match {
    case t@TypeRefType(thisType: ThisType, symbol, List()) =>
      //val result = thisType.symbol.name + "." + symbol.name
      symbol match {
        case classSymbol: ClassSymbol =>
          //val identifier = classSymbol.symbolInfo.owner.toString + "." + classSymbol.name
          SimpleType(getName(classSymbol))
        case o =>
          //SimpleType(s"unknown: ${o.getClass} ${o.toString}")
          SimpleType(o.toString)
      }
    case t@TypeRefType(thisType: ThisType, symbol, List(arg)) if symbol.path == "scala.<repeated>" =>
      Repeated(convert(arg))
    case t@TypeRefType(NoPrefixType, symbol, List()) =>
      SimpleType(s"HERE: $t")
      //SimpleType(symbol.path)
    case t@TypeRefType(NoPrefixType, symbol, typeArgs) =>
      //val generics = typeArgs.map(convert)
      //GenericType(symbol.path, generics)
      SimpleType(s"two ${symbol}")
    case ExistentialType(typeRef, symbols) =>
      // TODO: treat this differently
      SimpleType("existential")
    case SingleType(thisType: ThisType, objectSymbol: ObjectSymbol) =>
      val name = thisType.symbol.name + "." + objectSymbol.name
      SimpleType(s"singleType $name")
      /*
    case SingleType(thisType: ThisType, symbol) =>
      val name = thisType.symbol.name + "." + symbol.name
      SimpleType(s"singleType2 $name")
    case SingleType(singleType: SingleType, symbol) =>
      val name = singleType.symbol.name + "." + symbol.name
      SimpleType(s"singleType3 $name")
       */

    case trt:TypeRefType =>
      SimpleType(trt.symbol.path)
      //SimpleType(s"trt: ${trt.symbol.path}.${trt.symbol.name}")
    case at: AnnotatedType =>
      SimpleType(s"at: $at")
    case mt:MethodType =>
      SimpleType(s"mt: $mt")
    case ct: ConstantType =>
      SimpleType(s"ct $ct")
    case rf: RefinedType =>
      SimpleType(s"rf: $rf")
    case tt: ThisType =>
      SimpleType(s"tt: $tt")
    case st: SingleType =>
      SimpleType(st.symbol.path)
    case o =>
      println(o)
      ???
  }

  import org.json4s.scalap.scalasig.ScalaSig

  private def findVRA(classFile: ClassFile): Seq[ScalaSig] = {
    val attributes = classFile.attributes.map(a => ClassAttribute(a)(classFile))

    attributes.flatMap {
      case ClassAttribute.RuntimeVisibileAnnotations(annotations) =>
        import ClassAttribute.RuntimeVisibileAnnotation
        import ClassAttribute.RuntimeVisibileAnnotation.Str

        annotations.flatMap {
          case RuntimeVisibileAnnotation("Lscala/reflect/ScalaSignature;", Seq(Str("bytes", scalaSig))) =>
            import org.json4s.scalap.scalasig._
            import org.json4s.scalap.ByteCodecs
            val length = ByteCodecs.decode(scalaSig)
            val result = ScalaSigAttributeParsers.parse(ByteCode(scalaSig.take(length)))
            Option(result)
          // TODO: handle scala long signature
          case RuntimeVisibileAnnotation("Lscala/reflect/ScalaLongSignature;", _) =>
            Option.empty
          // TODO: handle down below cases
          case RuntimeVisibileAnnotation("Ljava/lang/FunctionalInterface;", _) =>
            Option.empty
          case RuntimeVisibileAnnotation("Ljava/lang/annotation/Retention;", _) =>
            Option.empty
          case RuntimeVisibileAnnotation("Ljava/lang/annotation/Target;", _) =>
            Option.empty
          case other =>
            println(other)
            Option.empty
        }
      case _ =>
        None
    }
  }
}
