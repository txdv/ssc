package ssc.classfile.higher

import ssc.span.Span
import ssc.misc.PrettyPrint
import ssc.classfile.{
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
  // https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.2.1
  case object Byte extends JavaType {
    val value: String = "B"
  }
  case object Short extends JavaType {
    val value: String = "S"
  }
  case object Int extends JavaType {
    val value: String = "I"
  }
  case object Long extends JavaType {
    // TODO: correct value
    val value: String = "J"
  }
  case object Float extends JavaType {
    // TODO: correct value
    val value: String = "F"
  }
  case object Double extends JavaType {
    val value: String = "D"
  }
  case object Char extends JavaType {
    val value: String = "C"
  }
  case object Void extends JavaType {
    val value: String = "V"
  }
  case object Boolean extends JavaType {
    val value: String = "Z"
  }

  val String = Class("java/lang/String")
  val Object = Class("java/lang/Object")
  val Integer = Class("java/lang/Integer")
  val JavaBoolean = Class("java/lang/Boolean")

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


  def descriptor(signature: Seq[JavaType]) = {
    signature.toList match {
      case Nil =>
        ???
      case returnType :: args =>
        "(" +
        args.map(_.value).mkString("") +
        ")" +
        returnType.value
    }
  }
}

sealed trait StackElement
object StackElement {
  case class Ref(ref: FieldRef) extends StackElement
  case class Type(jtype: JavaType) extends StackElement
}

case class StackFrame(offset: Int, elements: Seq[StackElement]) {
  def addOffset(additional: Int): StackFrame = {
    StackFrame(offset + additional, elements)
  }

  def addElement(element: StackElement): StackFrame = {
    StackFrame(offset, elements :+ element)
  }

  def add(additional: Int, element: StackElement): StackFrame = {
    StackFrame(offset + additional, elements :+ element)
  }
}

case class Code(
  stackSize: Int,
  localsCount: Int,
  ops: Seq[Op],
  stackMap: Seq[StackFrame]) {

  def +(other: Code): Code = {
    Code(
      Math.max(stackSize, other.stackSize),
      localsCount + other.localsCount,
      ops ++ other.ops,
      stackMap ++ other.stackMap.map(_.addOffset(codeSize)))
  }

  def +(otherOption: Option[Code]): Code = {
    otherOption match {
      case None =>
        this
      case Some(other) =>
        this + other
    }
  }

  def withStackSize(newStackSize: Int): Code = {
    this.copy(stackSize = newStackSize)
  }

  def addStackSize(additional: Int): Code = {
    withStackSize(stackSize + additional)
  }

  def withStackMap(newStackMap: Seq[StackFrame]): Code = {
    this.copy(stackMap = newStackMap)
  }

  val codeSize: Int = ops.map(Op.size).sum
}

object Code {
  val empty = Code(stackSize = 0, localsCount = 0, ops = Seq.empty, stackMap = Seq.empty)

  def op(op: Op, stackSize: Int = 0, localsCount: Int = 0): Code = {
    Code(stackSize, localsCount, ops = Seq(op), stackMap = Seq.empty)
  }

  def ops(ops: Seq[Op]): Code = {
    Code(stackSize = 0, localsCount = 0, ops, stackMap = Seq.empty)
  }
}


sealed trait OpConst

case class ConstString(value: String) extends OpConst

sealed trait Op

object Op {
  case object iadd extends Op

  case class aload(index: Int) extends Op
  case class astore(index: Int) extends Op
  case object Return extends Op
  case class invoke(method: MethodRef, invokeType: invoke.Type) extends Op
  object invoke {
    sealed trait Type
    case object special extends Type
    case object virtual extends Type
    case object static extends Type
  }
  case class getstatic(field: FieldRef) extends Op
  case class ldc(const: OpConst) extends Op
  case class iconst(value: Int) extends Op
  case class newobj(jclass: JavaType.Class) extends Op
  case object dup extends Op

  case class bipush(byte: Byte) extends Op

  case class if_acmpne(offset: Int) extends Op
  case class if_icmpne(offset: Int) extends Op
  case class goto(offset: Int) extends Op

  def size(op: Op): Int = {
    op match {
      case Op.iadd => 1
      case aload(index) =>
        if (index >= 0 && index <= 3) 1 else 2
      case invoke(_, itype) => 3
      case Op.Return => 1
      case getstatic(_) => 3
      case bipush(_) => 2
      case if_icmpne(_) => 3
      case iconst(value) =>
        if ((value >= 0 && value <= 5) || (value == -1)) 1 else 3
      case goto(_) => 3
      case ldc(_) => 2
      case _ =>
        println(s"missing: $op")
        ???
    }
  }
}

case class FieldRef(
  jclass: JavaType.Class,
  name: String,
  signature: Seq[JavaType])

case class MethodRef(
  jclass: JavaType.Class,
  name: String,
  signature: Seq[JavaType])
{
  def returnType: JavaType = signature.head
}

case class Method(
  name: String,
  signature: Seq[JavaType],
  access: Set[AccessFlag],
  code: Option[Code]) {

  def returnType: JavaType = signature.head

  def arguments: Seq[JavaType] = signature.drop(1)
}

object Method {
  val ObjectConstructor = MethodRef(
    jclass = JavaType.Class("java/lang/Object"),
    name = "<init>",
    signature = Seq(JavaType.Void))

  val DefaultConstructor = {
    val code = Code(
      stackSize = 1,
      localsCount = 1,
      ops = Seq(
        Op.aload(0),
        Op.invoke(ObjectConstructor, Op.invoke.special),
        Op.Return,
      ),
      stackMap = Seq.empty)

    Method(
      name = "<init>",
      signature = Seq(JavaType.Void),
      access = Set(AccessFlag.Public),
      code = Some(code))
  }

}

case class Class(
  version: Version,
  access: Set[ClassAccessFlag],
  thisClass: JavaType.Class,
  superClass: JavaType.Class,
  methods: Seq[Method],
  attributes: Seq[Attribute])

sealed trait ClassAccessFlag extends AccessFlag

object ClassAccessFlag {

  case object Public extends ClassAccessFlag {
    val value: Int = 0x0001
  }

  case object Final extends ClassAccessFlag {
    val value: Int = 0x0010
  }

  case object Super extends ClassAccessFlag {
    val value: Int = 0x0020
  }

  case object Interface extends ClassAccessFlag {
    val value: Int = 0x0200
  }

  case object Abstract extends ClassAccessFlag {
    val value: Int = 0x0400
  }

  case object Synthetic extends ClassAccessFlag {
    val value: Int = 0x1000
  }

  case object Annotation extends ClassAccessFlag {
    val value: Int = 0x2000
  }

  case object Enum extends ClassAccessFlag {
    val value: Int = 0x4000
  }

  val values = Set[ClassAccessFlag](
    Public,
    Final,
    Super,
    Interface,
    Abstract,
    Synthetic,
    Annotation,
    Enum,
  )

  def parse(accessFlags: Int): Set[ClassAccessFlag] = {
    values.filter(_.isSet(accessFlags))
  }

}


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

  case object Static extends AccessFlag { // not sure if this belongs here
    def value: Int = 0x0008
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
      access = ClassAccessFlag.parse(classFile.accessFlags),
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
          instructions.map(instr => convert(instr)(classFile)),
          stackMap = Seq.empty)
      }
    }
  }

  private def getMethodRef(index: Int)(implicit classFile: ClassFile): MethodRef = {
    val ref = classFile.constAs[Constant.MethodRef](index)

    val nameAndType = ref.constNameAndType

    MethodRef(
      JavaType.Class(ref.constClass.stringName),
      nameAndType.stringName,
      signature = lastElementAsFirst(JavaType.parse(nameAndType.stringType)))
  }

  private def getOpConst(index: Int)(implicit classFile: ClassFile): OpConst = {
    classFile.const(index) match {
      case Constant.ConstString(index) =>
        ConstString(classFile.string(index))
      case c =>
        throw new RuntimeException(s"not supported: $c")
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

      case Instr.astore_0 =>
        Op.astore(0)
      case Instr.astore_1 =>
        Op.astore(1)
      case Instr.astore_2 =>
        Op.astore(2)
      case Instr.astore_3 =>
        Op.astore(3)

      case Instr.invokevirtual(index) =>
        Op.invoke(getMethodRef(index), Op.invoke.virtual)
      case Instr.invokespecial(index) =>
        Op.invoke(getMethodRef(index), Op.invoke.special)
      case Instr.Return =>
        Op.Return
      case Instr.getstatic(index) =>
        val ref = classFile.constAs[Constant.FieldRef](index)

        val nameAndType = ref.constNameAndType

        val fieldRef = FieldRef(
          JavaType.Class(ref.constClass.stringName),
          nameAndType.stringName,
          signature = JavaType.parse(nameAndType.stringType))

        Op.getstatic(fieldRef)

      case Instr.ldc(index) =>
        Op.ldc(getOpConst(index))
      case Instr.newobj(index) =>
        Op.newobj(JavaType.Class(classFile.klass(index).stringName))
      case Instr.dup =>
        Op.dup

      case Instr.if_acmpne(index) =>
        Op.if_acmpne(index)
      case Instr.goto(index) =>
        Op.goto(index)

      case instr =>
        throw new RuntimeException(s"not implemented: $instr")
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

  val cl = Converter.convert(classFile)
  PrettyPrint.pformat(cl)
}
