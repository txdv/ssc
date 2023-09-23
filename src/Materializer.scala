package ssc.classfile.higher

import ssc.misc.{ByteBufferStream}

import java.nio.ByteBuffer

sealed trait Constant

object Constant {
  case class Class(name: Utf8) extends Constant
  case class Utf8(value: String) extends Constant
  case class NameAndType(name: Utf8, descriptor: Utf8) extends Constant
  case class MethodRef(klass: Class, nameAndType: NameAndType) extends Constant
  case class FieldRef(klass: Class, nameAndType: NameAndType) extends Constant
  case class ConstString(str: Utf8) extends Constant
  case class ConstInt(int: Int) extends Constant
}

case class ClassFile(
  name: String,
  head: ByteBuffer,
  body: ByteBuffer,
) {
  def getBytes: Array[Byte] = {
    val headLength = head.limit()
    val bodyLength = body.limit()

    val length: Int = headLength + bodyLength

    val result = new Array[Byte](length)

    // src, srcPos, dest, destPos, length
    System.arraycopy(
      head.array(),
      0,
      result,
      0,
      headLength)

    System.arraycopy(
      body.array(),
      0,
      result,
      headLength,
      bodyLength)

    result
  }
}

// https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.3
class Materializer {

  val head = new ByteBufferStream

  var constant_index: Int = 1

  private val hash = new scala.collection.mutable.HashMap[Constant, Short]

  private def const(constant: Constant): Short = {
    hash.get(constant).getOrElse {
      constant match {
        case Constant.Utf8(name) =>
          head.putByte(1)
          head.putShort(name.length)
          head.putString(name)
        case klass: Constant.Class =>
          val name_index = const(klass.name)
          head.putByte(7)
          head.putShort(name_index)
        case ref: Constant.FieldRef =>
          val class_index = const(ref.klass)
          val iNameAndType = const(ref.nameAndType)
          head.putByte(9)
          head.putShort(class_index)
          head.putShort(iNameAndType)
        case mref: Constant.MethodRef =>
          val class_index = const(mref.klass)
          val iNameAndType = const(mref.nameAndType)
          head.putByte(10)
          head.putShort(class_index)
          head.putShort(iNameAndType)
        case nt: Constant.NameAndType =>
          val name_index = const(nt.name)
          val descriptor_index = const(nt.descriptor)
          head.putByte(12)
          head.putShort(name_index)
          head.putShort(descriptor_index)
        case Constant.ConstString(utf8) =>
          val string_index = const(utf8)
          head.putByte(8)
          head.putShort(string_index)
        case Constant.ConstInt(n) =>
          head.putByte(3)
          head.putInt(n)
      } 

      hash.put(constant, constant_index.toShort)
      val result = constant_index.toShort
      constant_index += 1
      result
    }
  }

  def bytes(jclass: Class): ClassFile = {
    head.putInt(0xCAFEBABE)
    head.putShort(jclass.version.minor)
    head.putShort(jclass.version.major)
    val constantPoolCount = head.reserveShort

    // write constants
    val body = write(jclass)

    constantPoolCount.putShort(constant_index.toShort)

    ClassFile(jclass.thisClass.name, head.getBytes, body.getBytes)
  }

  private def write(jclass: Class): ByteBufferStream = {
    val body = new ByteBufferStream

    body.putShort(jclass.access.map(_.value).foldLeft(0)(_ | _))
    body.putShort(const(convert(jclass.thisClass)))
    body.putShort(const(convert(jclass.superClass)))
    // TODO: interfaces_count
    body.putShort(0)
    // TODO: fields_count
    body.putShort(0)

    body.putShort(jclass.methods.size)

    jclass.methods.foreach { method =>
      write(method, body)
    }

    // TODO: attributes_count
    body.putShort(0)

    body
  }

  private def write(method: Method, body: ByteBufferStream): Unit = {
    body.putShort(method.access.map(_.value).foldLeft(0)(_ | _))
    body.putShort(const(Constant.Utf8(method.name)))

    body.putShort(const(Constant.Utf8(JavaType.descriptor(method.signature))))

    body.putShort(method.code.size)

    method.code.foreach { code =>
      body.putShort(const(Constant.Utf8("Code")))
      val attributeLength = body.trackLength

      body.putShort(code.stackSize)
      body.putShort(code.localsCount)
      val codeLength = body.trackLength

      code.ops.foreach { op =>
        val bytes = opBytes(op)
        body.putBytes(bytes)
      }

      codeLength.resolve()

      // exception_table_length
      body.putShort(0)


      if (code.stackMap.size > 0) {
        body.putShort(1)
        writeCodeAttributes(body, method, code)
      } else {
        body.putShort(0)
      }

      attributeLength.resolve()
    }
  }

  private def writeCodeAttributes(body: ByteBufferStream, method: Method, code: Code): Unit = {

    body.putShort(const(Constant.Utf8("StackMapTable")))

    val attributeLength = body.trackLength

    body.putShort(code.stackMap.size)

    val stackMap = StackFrame(-1, Seq()) +: code.stackMap

    stackMap.sliding(2).foreach {
      case List(StackFrame(start, _), StackFrame(end, List(t))) if end - start < 65 =>
        val offset = end - start - 1
        body.putByte(offset + 64)
        writeElement(body, t)
      case List(StackFrame(start, _), StackFrame(end, stack)) =>
        body.putByte(255) // type
        body.putShort(end - start - 1) // offset delta

        body.putShort(1) // number_of_locals

        val stringArray = JavaType.Array(JavaType.String)
        val stringArrayType = StackElement.Type(stringArray)

        writeElement(body, stringArrayType)

        body.putShort(stack.size)
        stack.foreach { stackElement =>
          writeElement(body, stackElement)
        }
    }
    attributeLength.resolve()
  }

  private def writeElement(body: ByteBufferStream, element: StackElement): Unit = element match {
    case StackElement.Type(jtype) =>
      jtype match {
        case jclass: JavaType.Class =>
          body.putByte(7)
          val target = const(convert(jclass))
          body.putShort(target)
        case JavaType.Int =>
          body.putByte(1)
        case jarray: JavaType.Array =>
          body.putByte(7)
          val target = const(convert(jarray))
          body.putShort(target)
        case _ => ???
      }
    case _ =>
      ???
  }

  def opBytes(op: Op): Array[Byte] = {
    import Op._
    op match {
      case aload(index) =>
        index match {
          case 0 => ByteArray(0x2a)
          case _ => ???
        }
      case invoke(methodref, itype) =>
        val code = itype match {
          case invoke.virtual => 0xb6
          case invoke.special => 0xb7
          case invoke.static => 0xb8
        }
        val index = const(convert(methodref))
        ByteArray(code, (index >> 8) & 0xff, index & 0xff)
      case Return =>
        ByteArray(0xb1)
      case getstatic(field) =>
        val index = const(convert(field))
        // TODO: can be wide
        ByteArray(0xb2, (index >> 8) & 0xff, index & 0xff)
      case ldc(ConstString(str)) =>
        val index = const(Constant.ConstString(Constant.Utf8(str)))
        ByteArray(0x12, index & 0xff)
      case iconst(value) =>
        value match {
          case -1 => ByteArray(0x02)
          case 0 => ByteArray(0x03)
          case 1 => ByteArray(0x04)
          case 2 => ByteArray(0x05)
          case 3 => ByteArray(0x06)
          case 4 => ByteArray(0x07)
          case 5 => ByteArray(0x08)
          case n => ByteArray(0x12, const(Constant.ConstInt(n)))
        }
      case Op.iadd =>
        ByteArray(0x60)
      case bipush(byte) =>
        ByteArray(0x10, byte)
      case if_icmpne(offset) =>
         ByteArray(0xa0, (offset >> 8) & 0xff, offset & 0xff)
      case goto(offset) =>
         ByteArray(0xa7, (offset >> 8) & 0xff, offset & 0xff)
      case _ =>
        println(s"missing op: $op")
        ???
    }
  }

  def ByteArray(bytes: Int*): Array[Byte] = {
    bytes.map(_.toByte).toArray
  }

  def convert(jclass: JavaType.Class): Constant.Class = {
    Constant.Class(Constant.Utf8(jclass.namespace))
  }

  def convert(jarray: JavaType.Array): Constant.Class = {
    Constant.Class(Constant.Utf8(jarray.value))
  }

  def convert(methodRef: MethodRef): Constant.MethodRef = {
    val descriptor = JavaType.descriptor(methodRef.signature)

    Constant.MethodRef(
      convert(methodRef.jclass),
      Constant.NameAndType(Constant.Utf8(methodRef.name), Constant.Utf8(descriptor)))
  }

  def convert(fieldRef: FieldRef): Constant.FieldRef = {
    //val descriptor = JavaType.descriptor(fieldRef.signature)
    val descriptor = fieldRef.signature.head.value

    Constant.FieldRef(
      convert(fieldRef.jclass),
      Constant.NameAndType(Constant.Utf8(fieldRef.name), Constant.Utf8(descriptor)))
  }
}
