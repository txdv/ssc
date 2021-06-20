package ssc.classfile

import java.nio.ByteBuffer

case class CodeAttribute(
  maxStack: Short,
  maxLocals: Short,
  instructions: Seq[Instr],
)

sealed trait Instr {
  val opcode: Byte
  val value: Array[Byte]
  lazy val size: Int = value.size
}

object Instr {
  abstract class Single(val opcodeInt: Int) extends Instr {
    val opcode: Byte = opcodeInt.toByte
    val value: Array[Byte] = Array[Byte](opcode.toByte)
  }

  case object Return extends Single(0xB1)

  case object iconst_0 extends Single(0x03)
  case object iconst_1 extends Single(0x04)
  case object iconst_2 extends Single(0x05)
  case object iconst_3 extends Single(0x06)
  case object iconst_4 extends Single(0x07)
  case object iconst_5 extends Single(0x08)

  case object aload_0 extends Single(0x2A)
  case object aload_1 extends Single(0x2B)
  case object aload_2 extends Single(0x2C)
  case object aload_3 extends Single(0x2C)

  case object astore_0 extends Single(0x4B)
  case object astore_1 extends Single(0x4C)
  case object astore_2 extends Single(0x4D)
  case object astore_3 extends Single(0x4E)

  abstract class Index(opcodeInt: Int, val idx: Int) extends Instr {
    val opcode: Byte = opcodeInt.toByte

    val value: Array[Byte] = Array[Byte](
      opcode.toByte,
      ((idx     ) & 0xff).toByte,
      ((idx >> 8) & 0xff).toByte)
  }

  abstract class branch(opcodeInt: Int, val branchoffset: Int) extends Instr {
    val opcode: Byte = opcodeInt.toByte

    val value: Array[Byte] = Array[Byte](
      opcode.toByte,
      ((branchoffset     ) & 0xff).toByte,
      ((branchoffset >> 8) & 0xff).toByte)
  }

  // TODO: this is a single byte index
  case class ldc(index: Int) extends Index(0x12, index)

  case class getstatic(index: Int) extends Index(0xB2, index)
  case class invokevirtual(index: Int) extends Index(0xB6, index)
  case class invokespecial(index: Int) extends Index(0xB7, index)
  case class invokestatic(index: Int) extends Index(0xB8, index)

  case class if_icmpne(branch: Int) extends branch(0xA0, branch)
  case class if_acmpne(branch: Int) extends branch(0xA6, branch)
  case class goto(branch: Int) extends branch(0xA7, branch)
  case class ifnonnull(branch: Int) extends branch(0xC7, branch)

  case class newobj(index: Int) extends Index(0xBB, index)
  case object dup extends Single(0x59)

  def parse(bytes: Array[Byte]): Seq[Instr] = {
    val buffer = ByteBuffer.wrap(bytes)

    var acc = Vector[Instr]()

    while (buffer.remaining > 0) {
      acc = acc :+ parse(buffer)
    }

    acc
  }

  def parse(buffer: ByteBuffer): Instr = {
    val b = java.lang.Byte.toUnsignedInt(buffer.get)
    b match {
      case 0x03 => iconst_0
      case 0x04 => iconst_1
      case 0x05 => iconst_2
      case 0x06 => iconst_3
      case 0x07 => iconst_4
      case 0x12 => ldc(b & 0xFF)
      case 0x2A => aload_0
      case 0x2B => aload_1
      case 0x2C => aload_2
      case 0x2D => aload_3
      case 0x4B => astore_0
      case 0x4C => astore_1
      case 0x4D => astore_2
      case 0x4E => astore_3
      case 0x59 => dup
      case 0xA0 => if_icmpne(buffer.getShort)
      case 0xA6 => if_acmpne(buffer.getShort)
      case 0xA7 => goto(buffer.getShort)
      case 0xB1 => Return
      case 0xB2 => getstatic(buffer.getShort)
      case 0xB6 => invokevirtual(buffer.getShort)
      case 0xB7 => invokespecial(buffer.getShort)
      case 0xB8 => invokestatic(buffer.getShort)
      case 0xBB => newobj(buffer.getShort)
      case 0xC7 => ifnonnull(buffer.getShort)
      case _ =>
        println(s"missing: 0x${Hex.encode(b.toByte)}")
        ???
    }
  }
}

object CodeAttribute {
  def parse(classFile: ClassFile, attribute: AttributeInfo): Option[CodeAttribute] = {
    import classFile._
    val name = string(attribute.name)
    if (name != "Code") {
      None
    } else {
      val bb = ByteBuffer.wrap(attribute.info)
      val codeAttribute = CodeAttribute(
        maxStack = bb.getShort,
        maxLocals = bb.getShort,
        instructions = {
          val length = bb.getInt
          val byteArray = new Array[Byte](length)
          bb.get(byteArray)
          Instr.parse(byteArray)
        })

      Some(codeAttribute)
    }
  }

}

object Hex {
  def encode(bytes: Array[Byte]): String = {
    bytes.map(encode).mkString("")
  }

  def encode(b: Byte): String = {
    String.format("%02x", Byte.box(b))
  }
}
