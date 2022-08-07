package ssc.classfile

import ssc.span._
import java.nio.ByteBuffer

case class Version(minor: Int, major: Int)

sealed trait Constant {
  import Constant._

  def value(classFile: ClassFile): String = {
    this match {
      case MethodHandle(refKind, refIndex) =>
        s"MethodHandle($refKind, ${classFile.const(refIndex).value(classFile)})"
      case MethodType(idx) =>
        s"MethodType(${classFile.const(idx).value(classFile)})"
      case utf8: Utf8 =>
        s""""${utf8.string}""""
      case InterfaceMethodRef(klass, nameType) =>
        s"InterfaceMethodRef(${classFile.const(klass).value(classFile)}, ${classFile.const(nameType).value(classFile)})"
      case NameAndType(index, descriptor) =>
        s"NameAndType(${classFile.const(index).value(classFile)}, ${classFile.const(descriptor).value(classFile)})"
      case Class(name) =>
        classFile.const(name).value(classFile)
      case MethodRef(klass, nameType) =>
        s"MethodRef(${classFile.const(klass).value(classFile)}, ${classFile.const(nameType).value(classFile)})"
      case o =>
        o.toString
    }
  }

}

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
  case class Utf8(bytes: Array[Byte]) extends Constant {
    def string: String = new String(bytes)

    override def toString: String = {
      "Utf8(\"" + string + "\")"
    }
  }

  case class NameAndType(index: Int, descriptor: Int) extends Constant {
    def stringName(implicit classFile: ClassFile): String = {
      classFile.string(index)
    }

    def stringType(implicit classFile: ClassFile): String = {
      classFile.string(descriptor)
    }

    def string(implicit classFile: ClassFile): String = {
      s""""${stringName}":${stringType}"""
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
  case class MethodHandle(refKind: RefKind, refIndex: Int) extends Constant

  sealed trait RefKind
  object RefKind {
    case object GetField extends RefKind
    case object GetStatic extends RefKind
    case object PutField extends RefKind
    case object PutStatic extends RefKind
    case object InvokeVirtual extends RefKind
    case object InvokeStatic extends RefKind
    case object InvokeSpecial extends RefKind
    case object NewInvokeSpecial extends RefKind
    case object InvokeInterface extends RefKind

    def apply(value: Int): RefKind = value match {
      case 1 => GetField
      case 2 => GetStatic
      case 3 => PutField
      case 4 => PutStatic
      case 5 => InvokeVirtual
      case 6 => InvokeStatic
      case 7 => InvokeSpecial
      case 8 => NewInvokeSpecial
      case 9 => InvokeInterface
    }
  }

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
    utf8(index).string
  }

  def utf8(index: Int): Utf8 = {
    const(index).asInstanceOf[Utf8]
  }

  def klass(index: Int): Class = {
    const(index).asInstanceOf[Class]
  }

  def className(index: Int): String = {
    string(klass(index).name)
  }

  def nameAndType(index: Int): NameAndType = {
    const(index).asInstanceOf[NameAndType]
  }

  /*
  def nameAndType(index: Int): String = {
    val nt = const(index).asInstanceOf[NameAndType]
    nameAndType(nt)
    //s""""${string(a)}":${string(b)}"""
  }

  def nameAndType(nt: NameAndType): String = {
    val NameAndType(a, b) = nt
    s""""${string(a)}":${string(b)}"""
  }
  */

  def const(index: Int): Constant = {
    constants(index - 1)
  }

  def constAs[T <: Constant](index: Int): T  = {
    const(index).asInstanceOf[T]
  }

  def get(index: Int): String = {
    const(index) match {
      case utf8: Utf8 =>
        utf8.string
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

case class AttributeInfo(name: Int, info: Array[Byte]) {
  def getName(implicit classFile: ClassFile): String = {
    classFile.constants(name - 1).asInstanceOf[Utf8].string
  }

}

sealed trait ClassAttribute

object ClassAttribute {

  case class RuntimeVisibileAnnotations(annotations: Seq[RuntimeVisibileAnnotation]) extends ClassAttribute

  case class RuntimeVisibileAnnotation(annotationType: String, fields: Seq[RuntimeVisibileAnnotation.Field])

  object RuntimeVisibileAnnotation {
    sealed trait Field
    case class Str(name: String, content: Array[Byte]) extends Field
    case class Enum(enumType: String, value: String) extends Field
    case class Arr(fields: Seq[RuntimeVisibileAnnotation.Field]) extends Field
  }

  case class Signature(name: String) extends ClassAttribute

  case class SourceFile(name: String) extends ClassAttribute

  case object Scala extends ClassAttribute

  case class Info(name: String, info: Array[Byte]) extends ClassAttribute

  case class EnclosingMethod(klassName: String, method: Option[EnclosingMethod.Info]) extends ClassAttribute

  object EnclosingMethod {
    case class Info(name: String, methodType: String)
  }

  case class InnerClasses(classes: Seq[InnerClass]) extends ClassAttribute

  sealed trait InnerClass {
    val name: String
  }

  case class InnerClassRef(name: String, outer: String, outerName: String) extends InnerClass
  case class InnerClassSimple(name: String) extends InnerClass

  // TODO: type out with better types
  case class BootstrapMethods(methods: Seq[BootstrapMethod]) extends ClassAttribute
  case class BootstrapMethod(index: Int, values: Seq[Short])

  case class ScalaSig(major: Int, minor: Int) extends ClassAttribute

  case class ScalaInlineInfo(version: Int, traitImpl: Option[String], sam: Option[Sam], methods: Seq[InlineMethod]) extends ClassAttribute

  case class Sam(name: String, descriptor: String) extends ClassAttribute

  case class InlineMethod(name: String, descriptor: String, flags: Set[InlineMethodFlag])

  sealed trait InlineMethodFlag

  object InlineMethodFlag {
    case object Final extends InlineMethodFlag
    case object StaticImpl extends InlineMethodFlag
    case object HasInlineAnnotation extends InlineMethodFlag
    case object HasNoInlineAnnotation extends InlineMethodFlag

    def from(flags: Byte): Set[InlineMethodFlag] = {
      val isFinal = (flags & (1 << 0)) > 0
      val hasStaticImpl = (flags & (1 << 1)) > 0
      val hasInlineAnnotation = (flags & ( 1 << 2)) > 0
      val hasNoInlineAnnotation = (flags & ( 1 << 2)) > 0

      from(isFinal, hasStaticImpl, hasInlineAnnotation, hasNoInlineAnnotation)
    }

    def from(isFinal: Boolean, hasStaticImpl: Boolean, hasInlineAnnotation: Boolean, hasNoInlineAnnotation: Boolean): Set[InlineMethodFlag] = {
      Set.empty ++
        (if (isFinal) Option(Final) else Option.empty) ++
        (if (hasStaticImpl) Option(StaticImpl) else Option.empty) ++
        (if (hasInlineAnnotation) Option(HasInlineAnnotation) else Option.empty) ++
        (if (hasNoInlineAnnotation) Option(HasNoInlineAnnotation) else Option.empty)
    }
  }

  case object Deprecated extends ClassAttribute

  def apply(attributeInfo: AttributeInfo)(implicit classFile: ClassFile): ClassAttribute = {
    val info = attributeInfo.info
    val bb = ByteBuffer.wrap(info)

    attributeInfo.getName match {
      case "RuntimeVisibleAnnotations" =>
        val annotations = bb.getShort

        val result = (1 to annotations).map { _ =>
          val typeIndex = bb.getShort
          val annotationType = classFile.string(typeIndex)

          val numElementValuePairs = bb.getShort

          val fields = (1 to numElementValuePairs).map { _ =>
            val elementName = classFile.string(bb.getShort)
            parseElementValue(elementName, bb)
          }

          RuntimeVisibileAnnotation(annotationType, fields)
        }

        RuntimeVisibileAnnotations(result)
      case "Signature" =>
        ClassAttribute.Signature(classFile.string(bb.getShort))
      case "SourceFile" =>
        ClassAttribute.SourceFile(classFile.string(bb.getShort))
      case "Scala" =>
        Scala
      case "EnclosingMethod" =>
        val klassIndex = bb.getShort
        val klass = classFile.klass(klassIndex).stringName
        val methodIndex = bb.getShort

        val methodInfo =
          if (methodIndex == 0) {
            Option.empty
          } else {
            val method = classFile.nameAndType(methodIndex)
            Some(EnclosingMethod.Info(method.stringName, method.stringType))
          }

        EnclosingMethod(klass, methodInfo)

      case "InnerClasses" =>
        val numberOfClasses = bb.getShort
        //println(s"InnerClasses: $numberOfClasses")

        val klasses = (1 to numberOfClasses).map { _ =>
          val innerClassInfoIndex = bb.getShort
          val outerClassInfoIndex = bb.getShort
          val innerNameIndex = bb.getShort
          val innerClassAccessFlags = bb.getShort

          val innerClass = classFile.klass(innerClassInfoIndex).stringName

          if (outerClassInfoIndex == 0 && innerNameIndex == 0) {
            InnerClassSimple(innerClass)
          } else if (outerClassInfoIndex != 0 && innerNameIndex != 0) {
            val outerClass = classFile.klass(outerClassInfoIndex).stringName
            val innerName = classFile.string(innerNameIndex)
            InnerClassRef(innerClass, outerClass, innerName)
          } else if (outerClassInfoIndex == 0 && innerNameIndex != 0) {
            InnerClassSimple(innerClass)
          } else {
            println(s"outer: $outerClassInfoIndex inner: $innerNameIndex")
            ???
          }

        }

        InnerClasses(klasses)
        //Info("InnerClasses", attributeInfo.info)
      case "BootstrapMethods" =>
        val numBootstrapMethods = bb.getShort
        //println(s"numBootstrapMethods: $numBootstrapMethods")

        val methods = (1 to numBootstrapMethods).map { _ =>
          val bootstrapMethodRef = bb.getShort
          //println(classFile.const(bootstrapMethodRef).value(classFile))
          val numBootstrapArguments = bb.getShort
          val bootstrapArguments = (1 to numBootstrapArguments).map { _ => bb.getShort }
          //println(s"bootstrapArguments: $bootstrapArguments")
          //println(s"bootstrapArguments: ${bootstrapArguments.map(a => classFile.const(a).value(classFile))}")

          BootstrapMethod(bootstrapMethodRef, bootstrapArguments)
        }
        BootstrapMethods(methods)
      case "ScalaSig" =>
        val major, minor, _ = bb.get
        ScalaSig(major, minor)
      case "ScalaInlineInfo" =>
        val version = bb.get
        val flags = bb.get

        val effectivelyFinal = (flags & (1 << 0)) > 0
        val hasTraitImplClassSelfType = (flags & (1 << 1)) > 0
        val hasSam = (flags & (1 << 2)) > 0
        val hasLaterInterface = (flags & (1 << 3)) > 0

        val traitImplClassSelfType =
          if (!hasTraitImplClassSelfType) Option.empty
          else Option(classFile.string(bb.getShort))

        //val traitImplClassSelfType = classFile.const()
        val sam =
          if (hasSam) {
            val samName = classFile.string(bb.getShort)
            val samDescriptor = classFile.string(bb.getShort)
            Option(Sam(samName, samDescriptor))
          } else Option.empty


        val numMethodEntries = bb.getShort

        val methods = (1 to numMethodEntries).map { _ =>
          val name = classFile.string(bb.getShort)
          val descriptor = classFile.string(bb.getShort)
          val flags = bb.get

          val methodFlags = InlineMethodFlag.from(flags)

          InlineMethod(name, descriptor, methodFlags)
        }

        ScalaInlineInfo(version, traitImplClassSelfType, sam, methods)
      case "Deprecated" =>
        Deprecated
      case name =>
        Info(name, attributeInfo.info)
    }
  }

  def parseElementValue(elementName: String, bb: ByteBuffer)(implicit classFile: ClassFile): RuntimeVisibileAnnotation.Field = {

    val tag = bb.get.toChar
    tag match {
      case 's' =>
        val index = bb.getShort
        val content = classFile.utf8(index).bytes
        RuntimeVisibileAnnotation.Str(elementName, content)
      case 'e' =>
        val typeNameIndex = bb.getShort
        val constNameIndex = bb.getShort
        RuntimeVisibileAnnotation.Enum(classFile.string(typeNameIndex), classFile.string(constNameIndex))
      case '[' =>
        val num = bb.getShort
        val arr = (1 to num).map { _ =>
          parseElementValue(elementName, bb)
        }
        RuntimeVisibileAnnotation.Arr(arr)
      case _ =>
        println(tag)
        ???
    }
  }
}

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
        Utf8(bytes)
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
      case 0x0F => MethodHandle(RefKind(bb.get), bb.getShort)
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
