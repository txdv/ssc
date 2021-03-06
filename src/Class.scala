package lt.vu.mif.bentkus.bachelor.compiler.classfile.higher

import lt.vu.mif.bentkus.bachelor.compiler.span.Span
import lt.vu.mif.bentkus.bachelor.compiler.classfile.{
  AttributeInfo,
  Constant,
  Instr,
  ClassFile,
  CodeAttribute,
  MethodInfo,
  Version
}
import java.nio.ByteBuffer

sealed trait JavaType {

  val value: String

}

object JavaType {
  case object Int extends JavaType {
    val value: String = "I"
  }
  case object Char extends JavaType {
    val value: String = "C"
  }
  case object Void extends JavaType {
    val value: String = "V"
  }

  case class Class(namespace: String) extends JavaType {
    val tmp = namespace.split("/").reverse

    val pkg = tmp.drop(1).reverse.mkString("/")

    val name = tmp.head

    val value: String = "L" + namespace + ";"
  }

  object Class {
    def isChar(ch: Char): Boolean =
      ch.isLetterOrDigit || ch == '_' || ch == '/'
  }

  case class Array(info: JavaType) extends JavaType {
    val arity: Int = info match {
      case arr: Array => 1 + arr.arity
      case _ => 1
    }

    val value: String = "[" + info.value
  }


  def parse(str: String): Seq[JavaType] = {
    parse(str, types = Seq.empty)
  }

  private def parse(str: String, types: Seq[JavaType]): Seq[JavaType] = {
    if (str.size == 0) {
      types
    } else {
      str(0) match {
        case '(' | ')' =>
          parse(str.substring(1), types)
        case _ =>
          val t = from(str)
          parse(str.substring(t.value.size), types :+ t)

      }
    }
  }

  def from(a: String): JavaType = {
    a(0) match {
      case 'L' =>
        val until = a.indexOf(';')
        if (until == -1) {
          throw new Exception("invalid format")
        }
        Class(a.substring(1, until))
      case 'I' =>
        Int
      case 'C' =>
        Char
      case 'V' =>
        Void
      case '[' =>
        Array(from(a.substring(1)))
      case _ =>
        throw new Exception(s"invalid format ${a(0)}")
    }
  }
}

case class Code(
  stackSize: Int,
  localsCount: Int,
  operations: Seq[Op])


sealed trait Op

object Op {
  case class aload(index: Int) extends Op
  case object Return extends Op
  case class invokespecial(ref: MethodRef) extends Op
}

case class MethodRef(
  jclass: JavaType.Class,
  name: String,
  signature: Seq[JavaType])

case class Method(
  name: String,
  signature: Seq[JavaType],
  access: Set[AccessFlag],
  code: Option[Code]) {

  def returnType: JavaType = signature.head

  def arguments: Seq[JavaType] = signature.drop(1)
}

case class Class(
  version: Version,
  thisClass: JavaType.Class,
  superClass: JavaType.Class,
  methods: Seq[Method],
  attributes: Seq[Attribute])


sealed trait Attribute {
  val name: String
}

sealed trait KeyValueAttribute extends Attribute {
  val value: String

}
case class SignatureAttribute(name: String, value: String) extends KeyValueAttribute
case class SourceFileAttribute(name: String, value: String) extends KeyValueAttribute

case class GenericAttribute(name: String, value: Array[Byte]) extends Attribute

sealed trait AccessFlag {
  def value: Int

  def isSet(int: Int): Boolean = {
    (int & value) == value
  }
}

object AccessFlag {
  case object Public extends AccessFlag {
    val value: Int = 0x0001
  }

  case object Private extends AccessFlag {
    def value: Int = 0x0010
  }

  case object Super extends AccessFlag {
    def value: Int = 0x0020
  }

  case object Interface extends AccessFlag {
    def value: Int = 0x0200
  }

  case object Abstract extends AccessFlag {
    def value: Int = 0x0400
  }

  case object Annotation extends AccessFlag {
    def value: Int = 0x2000
  }

  case object Enum extends AccessFlag {
    def value: Int = 0x4000
  }

  val values = Set[AccessFlag](
    Public,
    Private,
    Super,
    Interface,
    Abstract,
    Annotation,
    Enum
  )

  def parse(af: Int): Set[AccessFlag] = {
    values.filter(_.isSet(af))
  }
}

sealed trait MethodAccessFlag {
  def value: Int

  def isSet(int: Int): Boolean = {
    (int & value) == value
  }
}

object MethodAccessFlag {
  case object Public extends MethodAccessFlag {
    val value: Int = 0x0001
  }

  case object Private extends MethodAccessFlag {
    val value: Int = 0x0002
  }

  case object Protected extends MethodAccessFlag {
    val value: Int = 0x0004
  }

  case object Static extends MethodAccessFlag {
    val value: Int = 0x0008
  }

  case object Final extends MethodAccessFlag {
    val value: Int = 0x0010
  }

  case object Synchronized extends MethodAccessFlag {
    val value: Int = 0x0020
  }

  case object Bridge extends MethodAccessFlag {
    val value: Int = 0x0040
  }

  case object Varargs extends MethodAccessFlag {
    val value: Int = 0x0080
  }

  case object Native extends MethodAccessFlag {
    val value: Int = 0x0100
  }

  case object Abstract extends MethodAccessFlag {
    val value: Int = 0x0400
  }

  case object Strict extends MethodAccessFlag {
    val value: Int = 0x0800
  }

  case object Synthetic extends MethodAccessFlag {
    val value: Int = 0x0800
  }

  val Values = Set[MethodAccessFlag](
    Public,
    Private,
    Protected,
    Static,
    Final,
    Synchronized,
    Bridge,
    Varargs,
    Native,
    Abstract,
    Strict,
    Synthetic
  )

  def parse(af: Int): Set[MethodAccessFlag] = {
    Values.filter(_.isSet(af))
  }
}

object Converter {
  def convert(classFile: ClassFile): Class = {
    val consts = classFile.constants

    val methods = classFile.methods.map { methodInfo =>
      import methodInfo._
      Method(
        name = classFile.string(name),
        signature = lastElementAsFirst(JavaType.parse(classFile.string(descriptor))),
        access = AccessFlag.parse(accessFlags),
        code = convertCode(classFile, attributes))
    }

    val classAttributes = classFile.attributes.map { attribute =>
      val name = classFile.string(attribute.name)
      val bb = ByteBuffer.wrap(attribute.info)
      name match {
        case "Signature" =>
          SignatureAttribute(name, classFile.string(bb.getShort))
        case "SourceFile" =>
          SourceFileAttribute(name, classFile.string(bb.getShort))
        case _ =>
          GenericAttribute(name, attribute.info)
      }
    }

    println(classFile.fields)

    Class(
      version = classFile.version,
      thisClass = JavaType.Class(classFile.className(classFile.thisClass)),
      superClass = JavaType.Class(classFile.className(classFile.superClass)),
      methods,
      classAttributes)
  }

  def convertCode(classFile: ClassFile, attributes: Seq[AttributeInfo]): Option[Code] = {
    attributes.find { attribute =>
      classFile.string(attribute.name) == "Code"
    } flatMap { attribute =>
      CodeAttribute.parse(classFile, attribute).map { codeAttribute =>
        import codeAttribute._
        Code(
          maxStack,
          maxLocals,
          instructions.map(instr => convert(instr)(classFile)))
      }
    }
  }

  def convert(instr: Instr)(implicit classFile: ClassFile): Op = {

    instr match {
      case Instr.aload_0 =>
        Op.aload(0)
      case Instr.aload_1 =>
        Op.aload(1)
      case Instr.aload_2 =>
        Op.aload(2)
      case Instr.aload_3 =>
        Op.aload(3)
      case Instr.invokespecial(index) =>
        val ref = classFile.constAs[Constant.MethodRef](index)

        val nameAndType = ref.constNameAndType

        val methodRef = MethodRef(
          JavaType.Class(ref.constClass.stringName),
          nameAndType.stringName,
          signature = JavaType.parse(nameAndType.stringType))

        Op.invokespecial(methodRef)
      case Instr.Return =>
        Op.Return
      case _ => throw new RuntimeException
    }
  }

  private def lastElementAsFirst[T](seq: Seq[T]): Seq[T] = {
    if (seq.size > 1) {
      seq.last +: seq.take(seq.size - 1)
    } else {
      seq
    }
  }
}

object MainApp extends App {

  val filename = args.head

  val bytes = {
    import java.io.File
    import java.nio.file.Files
    val file = new File(filename)
    Files.readAllBytes(file.toPath)
  }

  val classFile = ClassFile.parse(bytes)

  println(Converter.convert(classFile))
}
