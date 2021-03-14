package lt.vu.mif.bentkus.bachelor.compiler.classfile.higher

import lt.vu.mif.bentkus.bachelor.compiler.misc.{ByteBufferStream}

import java.nio.ByteBuffer

object Materializer {

  def bytes(jclass: Class): ByteBuffer = {
    val bb = new ByteBufferStream

    bb.putInt(0xCAFEBABE)
    bb.putShort(jclass.version.minor)
    bb.putShort(jclass.version.major)

    bb.getBytes
  }
}
