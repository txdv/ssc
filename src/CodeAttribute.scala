package lt.vu.mif.bentkus.bachelor.compiler.classfile

import java.nio.ByteBuffer

case class CodeAttribute(
  maxStack: Short,
  maxLocals: Short,
  code: Array[Byte],
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

  abstract class Index(opcodeInt: Int, index: Int) extends Instr {
    val opcode: Byte = opcodeInt.toByte

    val value: Array[Byte] = Array[Byte](
      opcode.toByte,
      (index & 0xff).toByte,
      ((index >> 8) & 0xff).toByte)
  }

  case class invokespecial(index: Int) extends Index(0xB7, index)

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
    if (b == 0xB1.toByte) {
      Return
    } else if (b == 0x2A.toByte) {
      aload_0
    } else if (b == 0xB7.toByte) {
      invokespecial(buffer.getShort)
    } else {
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
        code = {
          val length = bb.getInt
          val byteArray = new Array[Byte](length)
          bb.get(byteArray)
          byteArray
        })

      println(bb.getInt)

      Some(codeAttribute)
    }
  }

}
