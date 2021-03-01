package lt.vu.mif.bentkus.bachelor.compiler.classfile

import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Files

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
    constants(index -1).asInstanceOf[Class]
  }

  def className(index: Int): String = {
    string(klass(index).name)
  }
}

case class MethodInfo(accessFlags: Int, name: Int, descriptor: Int, attributes: Seq[AttributeInfo])
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

  def read(file: String): ClassFile = {
    val bytes = Files.readAllBytes(new File(file).toPath)

    ClassFile.parse(bytes)
  }

  args.headOption.map { file =>
    println(read(args.head))
  } getOrElse {
    println("Please pass file.")
  }
}
