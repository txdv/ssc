package lt.vu.mif.bentkus.bachelor.compiler.classfile.higher

import lt.vu.mif.bentkus.bachelor.compiler.classfile.{
  ClassFile,
  MethodInfo,
  Span,
  Version
}
import java.nio.ByteBuffer

sealed trait JavaType

case object JavaInt extends JavaType
case object JavaChar extends JavaType
case object JavaVoid extends JavaType

case class JavaClass(namespace: String) extends JavaType {
  private val tmp = namespace.split("/").reverse
  val pkg = tmp.drop(1).reverse.mkString("/")
  val name = tmp.head
}

object JavaClass {
  def isChar(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '_' || ch == '/'
}

case class JavaGeneric(namespace: String, genericType: JavaType)

object JavaType {
  def from(a: String): JavaType = {
    a(0) match {
      case 'L' =>
        JavaClass(a.drop(1).takeWhile(JavaClass.isChar))
      case '[' =>
        Arr(from(a.drop(1)))
      case 'I' =>
        JavaInt
      case 'C' =>
        JavaChar
      case 'V' =>
        JavaVoid
      case _ =>
        throw new Exception("invalid format")
    }
  }

  def method(str: String): Seq[JavaType] = {
    Seq.empty
  }
}

case class Arr(info: JavaType) extends JavaType {
  val arity: Int = info match {
    case arr: Arr => 1 + arr.arity
    case _ => 1 }
}

case class Method(
  name: String,
  taip: String,
  access: Set[AccessFlag])

case class Class(
  version: Version,
  thisClass: JavaClass,
  superClass: JavaClass,
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

    val methods = classFile.methods.map { case MethodInfo(accessFlags, name, descriptor, attributes) =>
      Method(
        name = classFile.string(name),
        taip = classFile.string(descriptor),
        access = AccessFlag.parse(accessFlags))
    }

    val attributes = classFile.attributes.map { attribute =>
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
      thisClass = JavaClass(classFile.className(classFile.thisClass)),
      superClass = JavaClass(classFile.className(classFile.superClass)),
      methods,
      attributes)
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
