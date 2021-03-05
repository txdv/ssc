package lt.vu.mif.bentkus.bachelor.compiler.span

case class Span(start: Int, end: Int, buffer: Array[Byte]) {

  def char(pos: Int): Char =
    buffer(pos).asInstanceOf[Char]

  def takeWhile(predicate: Char => Boolean): SpanSplit = {
    var i = start
    while (i < end && predicate(char(i))) {
      i += 1
    }
    split(i - start)
  }

  def split(prefix: Int): SpanSplit =
    SpanSplit(Span(start, start + prefix, buffer), Span(start + prefix, end, buffer))

  def takeChar: SpanSplit = split(1)

  def getString: String = getString(end - start)

  def getString(number: Int): String = {
    val end = scala.math.min(number, buffer.size)
    new String(buffer, start, end)
  }

  def char: Char = char(start)

  def isEmpty: Boolean = start >= end

  def withStart(f: Int => Int): Span =
    copy(start = f(start))

  def withEnd(f: Int => Int): Span =
    copy(end = f(end))

  def +(that: Span): Span = {
    if (buffer == that.buffer && end == that.start) {
      Span(start, that.end, buffer)
    } else {
      throw new Exception
    }
  }

  def +(that: SpanSplit) = that + this

  def toHex: String =
    buffer.map(Span.toHex).mkString
}

case class SpanSplit(prefix: Span, suffix: Span) {
  def map[T](f: Span => T): (T, Span) = (f(prefix), suffix)

  def move(step: Int): SpanSplit =
    SpanSplit(prefix.withEnd(_ + step), suffix.withStart(_ + step))

  def +(that: Span): SpanSplit = {
    SpanSplit(that + prefix, suffix)
  }
}

object Span {
  def apply(buffer: Array[Byte]): Span = {
    Span(0, buffer.length, buffer)
  }

  def toHex(b: Byte): String = {
    String.format("%02X", Byte.box(b))
  }

  def toHex(b: Array[Byte]): String =
    b.map(Span.toHex).mkString
}
