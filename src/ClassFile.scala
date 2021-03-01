package lt.vu.mif.bentkus.bachelor.compiler.classfile

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Files
import java.time.ZoneId
import java.time.format.DateTimeFormatter

case class Version(minor: Int, major: Int)

sealed trait Constant

object Constant {
  case class Class(name: Int) extends Constant
  case class MethodRef(klass: Int, nameType: Int) extends Constant
  case class FieldRef(klass: Int, nameType: Int) extends Constant
  case class Utf8(string: String) extends Constant
  case class NameAndType(index: Int, descriptor: Int) extends Constant
  case class ConstString(index: Int) extends Constant
}

import Constant._

case class ClassFile(
  version: Version,
  constants: Seq[Constant],
  accessFlags: Int,
  thisClass: Int,
  superClass: Int,
  fields: Seq[FieldInfo],
  methods: Seq[MethodInfo],
  attributes: Seq[AttributeInfo]) {

  def string(index: Int): String = {
    constants(index - 1).asInstanceOf[Utf8].string
  }

  def klass(index: Int): Class = {
    constants(index - 1).asInstanceOf[Class]
  }

  def className(index: Int): String = {
    string(klass(index).name)
  }

  def nameAndType(index: Int): String = {
    val nt = constants(index - 1).asInstanceOf[NameAndType]
    nameAndType(nt)
    //s""""${string(a)}":${string(b)}"""
  }

  def nameAndType(nt: NameAndType): String = {
    val NameAndType(a, b) = nt
    s""""${string(a)}":${string(b)}"""
  }
}

case class MethodInfo(
  accessFlags: Int,
  name: Int,
  descriptor: Int,
  attributes: Seq[AttributeInfo])

case class AttributeInfo(name: Int, info: Array[Byte])
case class FieldInfo(accessFlags: Int, name: Int, descriptor: Int, attributes: Seq[AttributeInfo])

object ClassFile {

  def parse(byte: Array[Byte]): ClassFile = {
    val bb = ByteBuffer.wrap(byte)
    val magic = bb.getInt
    val version = Version(bb.getShort, bb.getShort)

    val poolCount = bb.getShort

    val constants = (1 to poolCount - 1).map(_ => getConstant(bb))

    val accessFlags = bb.getShort
    val thisClass = bb.getShort
    val superClass = bb.getShort

    val interfacesCount = bb.getShort
    // TODO: read interfaces
    val fieldsCount = bb.getShort

    val fields = (1 to fieldsCount).map(_ => readFieldInfo(bb))

    val methodsCount = bb.getShort

    val methods = (1 to methodsCount).map { _ =>
      readMethodInfo(bb)
    }

    val attributes = readAttributes(bb)

    ClassFile(version, constants, accessFlags, thisClass, superClass, fields, methods, attributes)
  }

  private def readMethodInfo(bb: ByteBuffer): MethodInfo = {
    val accessFlags = bb.getShort
    val name = bb.getShort
    val descriptor = bb.getShort

    val attributes = readAttributes(bb)

    MethodInfo(accessFlags, name, descriptor, attributes)
  }

  private def readFieldInfo(bb: ByteBuffer): FieldInfo = {
    val accessFlags = bb.getShort
    val name = bb.getShort
    val descriptor = bb.getShort

    val attributes = readAttributes(bb)

    FieldInfo(accessFlags, name, descriptor, attributes)
  }

  def readAttributes(bb: ByteBuffer): Seq[AttributeInfo] = {
    val attributeCount = bb.getShort

    (1 to attributeCount).map { _ =>
      readAttribute(bb)
    }
  }

  def readAttribute(bb: ByteBuffer): AttributeInfo = {
    val name = bb.getShort
    val length = bb.getInt
    val info = new Array[Byte](length)
    bb.get(info)
    AttributeInfo(name, info)
  }

  def getConstant(bb: ByteBuffer): Constant = {
    bb.get match {
      case 0x01 => {
        val length = bb.getShort
        val bytes = new Array[Byte](length)
        bb.get(bytes)
        Utf8(new String(bytes))
      }
      case 0x0C => {
        NameAndType(bb.getShort, bb.getShort)
      }
      case 0x07 => Class(bb.getShort)
      case 0x08 => ConstString(bb.getShort)
      case 0x09 => FieldRef(bb.getShort, bb.getShort)
      case 0x0A => MethodRef(bb.getShort, bb.getShort)
      case i => throw new Exception(i + " -> " + Span.toHex(i))
    }
  }
}


case class Span(start: Int, end: Int, buffer: Array[Byte]) {
  def readByte: (Int, Span) = {
    (buffer(0) & 0xFF, skip(1))
  }

  def readInt: (Int, Span) = (getInt, skip(4))

  def getInt: Int = toByteBuffer.getInt

  def toByteBuffer: ByteBuffer = ByteBuffer.wrap(buffer, start, end - start)

  def skip(i: Int): Span = Span(start + i, end, buffer)

  def toHex: String =
    buffer.map(Span.toHex).mkString

  def format: String = {
    import scala.io.AnsiColor.{GREEN, RED, RESET}

    val prefix = toHex.substring(0, start * 2)

    val infix = toHex.substring(start * 2, end)

    val suffix = ""

    GREEN + prefix + RED + infix + GREEN + suffix + RESET
  }

  def apply(index: Int): Byte = {
    buffer(start + index)
  }
}


object Span {
  def apply(buffer: Array[Byte]): Span = {
    Span(0, buffer.length, buffer)
  }

  def toHex(b: Byte): String = {
    String.format("%02X", Byte.box(b))
  }

  def toHex(b: Array[Byte]): String =
    b.map(Span.toHex).mkString

}

object MainApp extends App {

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

  def print(classFile: ClassFile): Unit = {
    import classFile._
    val thisClassString = className(thisClass)
    println(s"public class $thisClassString")
    println(s"  minor version: ${version.minor}")
    println(s"  major version: ${version.major}")
    println(s"  flags: (${toHex(accessFlags)})")
    println(s"  this_class: #${thisClass} // $thisClassString")
    println(s"  super_class: #${superClass} // ${className(superClass)} ")
    println(s"  interfaces: 0, fields: 0, methods: ${methods.size}, attributes: 0")
    println("Constant pool:")
    constants.zipWithIndex.foreach { case (constant, i) =>
      val nr = i + 1
      constant match {
        case _ =>
          val nrstr = leftpad(s"#$nr", ' ')
          val name = rightpad(constant.getClass.getSimpleName, ' ', 18)
          val value = rightpad(toValue(constant), ' ', 15)
          val comments = toComment(classFile, constant).getOrElse("")
          println(s"$nrstr = $name $value $comments")
      }
    }
    println("{")
    methods.foreach { method =>
      val methodName = string(method.name)
      val af = method.accessFlags
      println(s"  $methodName")
    }

    println("}")
  }

  args.headOption.map { filename =>
    val file = new File(filename)
    val bytes = Files.readAllBytes(file.toPath)

    import java.nio.file.attribute.BasicFileAttributes

    val t = Files.readAttributes[BasicFileAttributes](file.toPath, classOf[BasicFileAttributes])
    val cf = ClassFile.parse(bytes)

    val date = formatter.format(t.lastModifiedTime.toInstant)
    println(s"  Last modified $date; size ${bytes.size} bytes")

    print(cf)
  } getOrElse {
    println("Please pass file.")
  }
}
