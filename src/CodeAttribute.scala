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
    val b = buffer.get
    if (b == 0x12.toByte) {
      ldc(buffer.get & 0xFF)
    } else if (b == 0x2A.toByte) {
      aload_0
    } else if (b == 0x2B.toByte) {
      aload_1
    } else if (b == 0x2C.toByte) {
      aload_2
    } else if (b == 0x2D.toByte) {
      aload_3
    } else if (b == 0x4B.toByte) {
      astore_0
    } else if (b == 0x4C.toByte) {
      astore_1
    } else if (b == 0x4D.toByte) {
      astore_2
    } else if (b == 0x4E.toByte) {
      astore_3
    } else if (b == 0x59.toByte) {
      dup
    } else if (b == 0xA6.toByte) {
      if_acmpne(buffer.getShort)
    } else if (b == 0xA7.toByte) {
      goto(buffer.getShort)
    } else if (b == 0xC7.toByte) {
      ifnonnull(buffer.getShort)
    } else if (b == 0xB1.toByte) {
      Return
    } else if (b == 0xB2.toByte) {
      getstatic(buffer.getShort)
    } else if (b == 0xB6.toByte) {
      invokevirtual(buffer.getShort)
    } else if (b == 0xB7.toByte) {
      invokespecial(buffer.getShort)
    } else if (b == 0xBB.toByte) {
      newobj(buffer.getShort)
    } else {
      val instr = b & 0xFF
      println(s"missing: 0x${Hex.encode(b)}")
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
