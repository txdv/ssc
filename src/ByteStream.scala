package lt.vu.mif.bentkus.bachelor.compiler.misc

import java.nio.ByteBuffer

import java.io.{ByteArrayOutputStream => MemoryStream}

trait ByteStream {
  def putByte(i: Int): Unit
  def putShort(i: Int): Unit
  def putInt(i: Int): Unit
  def putBytes(bytes: Array[Byte]): Unit

  def putString(string: String): Unit = {
    putBytes(string.getBytes)
  }

  def getBytes: ByteBuffer

  def reserveInt: ByteBuffer
  def reserveShort: ByteBuffer

  def trackLength: LengthTracker

  def position(): Int
}

trait LengthTracker {
  def resolve(): Unit
}

case class LengthTrackerImpl(bs: ByteStream) extends LengthTracker {
  private var resolved = false
  private var placeholder = bs.reserveInt
  private val start = bs.position()

  def resolve(): Unit = {
    if (resolved) {
      throw new Exception("already resolved")
    }
    val end = bs.position()
    val length = end - start

    resolved = true
    placeholder.putInt(length)
  }
}

class ByteBufferStream extends ByteStream {
  val buffer = new Array[Byte](1024 * 1024)
  val bb = ByteBuffer.wrap(buffer)

  def putByte(i: Int): Unit = {
    bb.put(i.toByte)
  }

  def putShort(i: Int): Unit = {
    bb.putShort(i.toShort)
  }

  def putInt(i: Int): Unit = {
    bb.putInt(i)
  }

  def putBytes(bytes: Array[Byte]): Unit = {
    bb.put(bytes, 0, bytes.length)
  }

  def getBytes: ByteBuffer = {
    ByteBuffer.wrap(buffer, 0, bb.position())
  }

  def reserveInt: ByteBuffer = {
    val result = ByteBuffer.wrap(bb.array, bb.position(), 4)
    bb.putInt(0)
    result
  }

  def reserveShort: ByteBuffer = {
    val result = ByteBuffer.wrap(bb.array, bb.position(), 2)
    bb.putShort(0)
    result
  }

  def position(): Int = {
    bb.position()
  }

  def trackLength: LengthTracker = {
    LengthTrackerImpl(this)
  }
}
