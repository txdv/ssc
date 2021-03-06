package lt.vu.mif.bentkus.bachelor.compiler.classfile

import lt.vu.mif.bentkus.bachelor.compiler.span._
import java.nio.ByteBuffer

case class Version(minor: Int, major: Int)

sealed trait Constant

object Constant {
  case class Class(name: Int) extends Constant {
    def stringName(implicit classFile: ClassFile): String = {
      classFile.string(name)
    }
  }
  case class MethodRef(klass: Int, nameType: Int) extends Constant {
    def constClass(implicit classFile: ClassFile): Class = {
      classFile.klass(klass)
    }

    def constNameAndType(implicit classFile: ClassFile): NameAndType = {
      classFile.constAs[NameAndType](nameType)
    }
  }
  case class FieldRef(klass: Int, nameType: Int) extends Constant
  case class Utf8(string: String) extends Constant
  case class NameAndType(index: Int, descriptor: Int) extends Constant {
    def stringName(implicit classFile: ClassFile): String = {
      classFile.string(index)
    }

    def stringType(implicit classFile: ClassFile): String = {
      classFile.string(descriptor)
    }
  }
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
    const(index).asInstanceOf[Utf8].string
  }

  def klass(index: Int): Class = {
    const(index).asInstanceOf[Class]
  }

  def className(index: Int): String = {
    string(klass(index).name)
  }

  def nameAndType(index: Int): String = {
    val nt = const(index).asInstanceOf[NameAndType]
    nameAndType(nt)
    //s""""${string(a)}":${string(b)}"""
  }

  def nameAndType(nt: NameAndType): String = {
    val NameAndType(a, b) = nt
    s""""${string(a)}":${string(b)}"""
  }

  def const(index: Int): Constant = {
    constants(index - 1)
  }

  def constAs[T <: Constant](index: Int): T  = {
    const(index).asInstanceOf[T]
  }

  def get(index: Int): String = {
    const(index) match {
      case Utf8(value) =>
        value
      case Class(index) =>
        get(index)
      case MethodRef(klass, nameType) =>
        s"Method ${get(klass)}, ${get(nameType)}"
      case FieldRef(klass, nameType) =>
        s"${get(klass)}, ${get(nameType)}"
      case NameAndType(klass, descriptor) =>
        s"${get(klass)}, ${get(descriptor)}"
      case ConstString(index) =>
        get(index)
    }
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
