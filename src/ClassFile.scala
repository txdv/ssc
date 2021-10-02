package ssc.classfile

import ssc.span._
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
  case class InterfaceMethodRef(klass: Int, nameType: Int) extends Constant {
    def constClass(implicit classFile: ClassFile): Class = {
      classFile.klass(klass)
    }

    def constNameAndType(implicit classFile: ClassFile): NameAndType = {
      classFile.constAs[NameAndType](nameType)
    }
  }
  case class FieldRef(klass: Int, nameType: Int) extends Constant {
    def constClass(implicit classFile: ClassFile): Class = {
      classFile.klass(klass)
    }

    def constNameAndType(implicit classFile: ClassFile): NameAndType = {
      classFile.constAs[NameAndType](nameType)
    }
  }
  case class Utf8(string: String) extends Constant
  case class NameAndType(index: Int, descriptor: Int) extends Constant {
    def stringName(implicit classFile: ClassFile): String = {
      classFile.string(index)
    }

    def stringType(implicit classFile: ClassFile): String = {
      classFile.string(descriptor)
    }
  }
  //  3 0x03
  case class ConstInteger(value: Int) extends Constant
  //  4 0x04
  case class ConstFloat(value: Float) extends Constant
  //  5 0x05
  case class ConstLong(value: Long) extends Constant
  //  6 0x06
  case class ConstDouble(value: Double) extends Constant
  //  8 0x08
  case class ConstString(index: Int) extends Constant
  // 15 0x0F
  case class MethodHandle(refKind: Byte, refIndex: Int) extends Constant
  // 16 0x10
  case class MethodType(descriptorIndex: Int) extends Constant
  case class InvokeDynamic(methodIndex: Short, nameTypeIndex: Int) extends Constant
  // 19 0x13
  case class Module(name: Short) extends Constant
  // 20 0x14
  case class Package(name: Short) extends Constant
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
      case InterfaceMethodRef(klass, nameType) =>
        s"InterfaceMethod ${get(klass)}, ${get(nameType)}"
      case FieldRef(klass, nameType) =>
        s"${get(klass)}, ${get(nameType)}"
      case NameAndType(klass, descriptor) =>
        s"${get(klass)}, ${get(descriptor)}"
      case ConstInteger(value) =>
        s"ConstInteger($value)"
      case ConstLong(value) =>
        s"ConstLong($value)"
     case ConstFloat(value) =>
        s"ConstFloat($value)"
      case ConstDouble(value) =>
        s"ConstDouble($value)"
      case ConstString(index) =>
        get(index)
      case MethodHandle(kind, index) =>
        s"MethodHandle($kind, $index)"
      case MethodType(index) =>
        s"MethodType($index)"
      case InvokeDynamic(method, nameAndType) =>
        s"InvokeDynamic($method, $nameAndType)"
      case Module(name) =>
        s"Module($name)"
      case Package(name) =>
        s"Package($name)"
    }
  }
}

case class MethodInfo(
  accessFlags: Int,
  name: Int,
  descriptor: Int,
  attributes: Seq[AttributeInfo]) {

  def getName(implicit classFile: ClassFile): String = {
    classFile.constants(name - 1).asInstanceOf[Utf8].string
  }

  def getDescriptor(implicit classFile: ClassFile): String = {
    classFile.constants(descriptor - 1).asInstanceOf[Utf8].string
  }
}

case class AttributeInfo(name: Int, info: Array[Byte])
case class FieldInfo(accessFlags: Int, name: Int, descriptor: Int, attributes: Seq[AttributeInfo])

object ClassFile {

  def parse(byte: Array[Byte]): ClassFile = {
    val bb = ByteBuffer.wrap(byte)
    val magic = bb.getInt
    val version = Version(bb.getShort, bb.getShort)

    val poolCount = bb.getShort

    //println(s"constants: $poolCount")

    //val o = Vector(1)
    //val constants = (1 to poolCount - 1).map(i => getConstant(i, bb))
    val constants = Array.ofDim[Constant](poolCount - 1)

    {
      var i = 0
      while (i < constants.length) {
        val c = getConstant(i, bb)
        constants(i) = c
        i += 1

        if (c.isInstanceOf[ConstLong] || c.isInstanceOf[ConstDouble]) {
          constants(i) = c
          i += 1
        }
      }
    }

    val accessFlags = bb.getShort
    val thisClass = bb.getShort
    val superClass = bb.getShort

    val interfacesCount = bb.getShort
    val interfaces = (1 to interfacesCount).map { _ => bb.getShort }
    //println(s"interfaces: $interfaces")
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

  def getConstant(i: Int, bb: ByteBuffer): Constant = {
    val tag = bb.get
    val result = tag match {
      case 0x01 => {
        val length = bb.getShort & 0xffff
        val bytes = new Array[Byte](length)
        bb.get(bytes)
        Utf8(new String(bytes))
      }
      case 0x03 => ConstInteger(bb.getInt)
      case 0x04 => ConstFloat(bb.getFloat)
      case 0x05 => ConstLong(bb.getLong)
      case 0x06 => ConstDouble(bb.getDouble)
      case 0x07 => Class(bb.getShort)
      case 0x08 => ConstString(bb.getShort)
      case 0x09 => FieldRef(bb.getShort, bb.getShort)
      case 0x0A => MethodRef(bb.getShort, bb.getShort)
      case 0x0B => InterfaceMethodRef(bb.getShort, bb.getShort)
      case 0x0C => {
        NameAndType(bb.getShort, bb.getShort)
      }
      case 0x0F => MethodHandle(bb.get, bb.getShort)
      case 0x10 => MethodType(bb.getShort)
      case 0x12 => InvokeDynamic(bb.getShort, bb.getShort)
      case 0x13 => Module(bb.getShort)
      case 0x14 => Package(bb.getShort)
      case i => throw new Exception(s"unknown i 0x${Span.toHex(i)} at pos ${bb.position()}")
    }

    //println(s"#$i $tag $result")

    result
  }
}
