package lt.vu.mif.bentkus.bachelor.compiler.classfile

import lt.vu.mif.bentkus.bachelor.compiler.classfile.higher.MethodAccessFlag
import lt.vu.mif.bentkus.bachelor.compiler.classfile.higher.JavaType
import Constant._

import java.io.File
import java.nio.file.Files
import java.time.ZoneId
import java.time.format.DateTimeFormatter

object ByteArray {
  implicit class Extensions(array: Array[Byte]) {

    def formatHex: String = {
      array.map(Hex.encode).mkString(" ")
    }
  }
}

import ByteArray.Extensions

object Hex {
  def encode(bytes: Array[Byte]): String = {
    bytes.map(encode).mkString("")
  }

  def encode(b: Byte): String = {
    String.format("%02x", Byte.box(b))
  }
}

object SHA256 {
  lazy val digest = java.security.MessageDigest.getInstance("SHA-256")

  def sum(bytes: Array[Byte]): String = {
    val output = digest.digest(bytes)
    new String(Hex.encode(output))
  }
}

object ClassFileApp extends App {

  val formatter = DateTimeFormatter.ofPattern("LLL d, YYYY")
    .withZone(ZoneId.systemDefault())

  def toHex(i: Int): String = {
    String.format("0x%04X", i)
  }

  def repeat(char: Char, times: Int): String = {
    (1 to times).map(_ => char).mkString("")
  }

  def leftpad(str: String, filler: Char = ' ', size: Int = 5): String = {
    val i = Math.max(size - str.length, 0)
    repeat(filler, i) + str
  }

  def rightpad(str: String, filler: Char = ' ', size: Int = 19): String = {
    val i = Math.max(size - str.length, 0)
    str + repeat(filler, i)
  }

  def toValue(c: Constant): String = {

    c match {
      case MethodRef(klass, nameType) =>
        s"#$klass.#$nameType"
      case Class(i) =>
        s"#$i"
      case NameAndType(index, descriptor) =>
        s"#$index:#$descriptor"
      case Utf8(str) =>
        str
      case _ =>
        c.toString
    }
  }

  def toComment(classFile: ClassFile, constant: Constant): Option[String] = {
    import classFile._

    constant match {
      case MethodRef(klass, nameAndTypeId) =>
        val b = nameAndType(nameAndTypeId)
        val value = s"${className(klass)}.${b}"
        Some("// ").map(_ + value)
        //Some(s"// $value")
      case Class(index) =>
        val value = string(index)
        Some("// ").map(_ + value)
      case n: NameAndType =>
        val value = nameAndType(n)
        Some("// ").map(_ + value)
      case _ =>
        None
    }
  }

  def keywords(flags: Set[MethodAccessFlag]): String = {
    flags.toSeq.
      sortBy(_.value)
      .map(_.toString.toLowerCase)
      .mkString(" ")
  }

  private def format(jtype: JavaType): String = {
    import JavaType._
    jtype match {
      case Void =>
        "void"
      case Int =>
        "int"
      case c: Class =>
        c.namespace.replace("/", ".")
      case array: Array =>
        format(array.info) + "[]"
      case _ =>
        "Not supported"
    }
  }

  def indent(offset: Int): Unit = {
    print("  " * offset)
  }

  def indent(offset: Int, suffix: String): Unit = {
    indent(offset)
    println(suffix)
  }

  def output(offset: Int, classFile: ClassFile, instr: Instr): Unit = {
    indent(5)
    print(s"$offset: ")
    output(classFile, instr)
  }

  def output(classFile: ClassFile, instr: Instr): Unit = {
    import Instr._
    instr match {
      case index: Index =>
        val name = index.getClass.getSimpleName
        val suffix = toComment(classFile, classFile.const(index.idx)).getOrElse("")
        println(rightpad(s"$name #${index.idx}", ' ', 40) + suffix)
      case Return =>
        println("return")
      case _ =>
        println(instr)
    }

  }

  private def format(methodName: String, types: Seq[JavaType]): String = {
    val returnType = types.last
    val args = types.reverse.drop(1).reverse

    val returnTypeString = format(returnType)
    val argsString = args.map(format).mkString(", ")

    s"$returnTypeString $methodName($argsString);"
  }

  def printClassFile(classFile: ClassFile): Unit = {
    import classFile._
    val thisClassString = className(thisClass)
    indent(0, s"public class $thisClassString")
    indent(1, s"minor version: ${version.minor}")
    indent(1, s"major version: ${version.major}")
    indent(1, s"flags: (${toHex(accessFlags)})")
    indent(1, rightpad(s"this_class: #${thisClass}",   ' ', 40) + s"// $thisClassString")
    indent(1, rightpad(s"super_class: #${superClass}", ' ', 40) + s"// ${className(superClass)}")
    indent(1, s"interfaces: 0, fields: 0, methods: ${methods.size}, attributes: ${attributes.size}")
    indent(0, "Constant pool:")
    constants.zipWithIndex.foreach { case (constant, i) =>
      val nr = i + 1
      constant match {
        case _ =>
          val nrstr = leftpad(s"#$nr", ' ')
          val name = rightpad(constant.getClass.getSimpleName, ' ', 18)
          val value = rightpad(toValue(constant), ' ', 14)
          val comments = toComment(classFile, constant).getOrElse("")
          println(s"$nrstr = $name $value $comments")
      }
    }
    println("{")
    methods.foreach { method =>
      val methodName = string(method.name) match {
        case "<init>" => thisClassString
        case str => str
      }

      val flags = MethodAccessFlag.parse(method.accessFlags)
      val kw = keywords(flags)
      val descriptorString = string(method.descriptor)
      val annotation = format(methodName, JavaType.parse(descriptorString))
      println(s"  $kw $annotation")
      println(s"    descriptor: $descriptorString")

      val stringFlags = flags.toSeq.sortBy(_.value).map(s => s"ACC_${s.toString.toUpperCase}").mkString(", ")
      println(s"    flags: (${toHex(method.accessFlags)}) $stringFlags")

      method.attributes.foreach { attribute =>
        val name = string(attribute.name)
        if (name == "Code") {
          val size = attribute.info.size
          CodeAttribute.parse(classFile, attribute).foreach { code =>
            indent(2, "Code:")
            indent(3, s"stack=${code.maxStack}, locals=${code.maxLocals}, args_size = ?")
            //println(s"      ")
            var offset = 0
            code.instructions.foreach { instr =>
              output(offset, classFile, instr)
              offset += instr.size
            }
          }
          println
        } else {
          throw new Exception(s"$name attributes are not implemented")
        }
      }
    }

    println
    println("}")
  }

  args.headOption.map { filename =>
    val file = new File(filename)
    println(s"Classfile: ${file.getAbsolutePath}")
    val bytes = Files.readAllBytes(file.toPath)

    import java.nio.file.attribute.BasicFileAttributes

    val t = Files.readAttributes[BasicFileAttributes](file.toPath, classOf[BasicFileAttributes])
    val cf = ClassFile.parse(bytes)

    val date = formatter.format(t.lastModifiedTime.toInstant)
    indent(1, s"Last modified $date; size ${bytes.size} bytes")
    indent(1, s"SHA-256 checksum ${SHA256.sum(bytes)}")

    printClassFile(cf)
  } getOrElse {
    println("Please pass file.")
  }
}
