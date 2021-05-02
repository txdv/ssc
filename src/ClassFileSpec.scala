package lt.vu.mif.bentkus.bachelor.compiler.classfile

import org.scalatest._
import flatspec._
import matchers._

class ClassSpec extends AnyFlatSpec with should.Matchers {
  import Instr._

  def arr(ints: Int*): Array[Byte] = {
    ints.map(_.toByte).toArray
  }

  "Instr" should "parse" in {
    parse(arr(0xB1)) should be (
      Seq(Return)
    )
    
    parse(arr(0x2A, 0xB7, 0x00, 0x01, 0xB1)) should be (
      Seq(aload_0, invokespecial(1), Return)
    )
  }
}
