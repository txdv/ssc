package lt.vu.mif.bentkus.bachelor.compiler.classfile.higher

import lt.vu.mif.bentkus.bachelor.compiler.misc.{ByteBufferStream}

import java.nio.ByteBuffer

// https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.3
object Materializer {


  def bytes(jclass: Class): (ByteBuffer, ByteBuffer) = {
    val head = new ByteBufferStream


    val body = write(jclass, head)

    head.putInt(0xCAFEBABE)
    head.putShort(jclass.version.minor)
    head.putShort(jclass.version.major)
    // write constants

    (head.getBytes, body.getBytes)
  }

  private def write(jclass: Class, head: ByteBufferStream): ByteBufferStream = {
    val body = new ByteBufferStream

    body.putShort(jclass.access.map(_.value).foldLeft(0)(_ | _))

    body
  }
}
