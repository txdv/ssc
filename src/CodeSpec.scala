package ssc.classfile.higher


import org.scalatest._
import flatspec._
import matchers._

class CodeSpec extends AnyFlatSpec with should.Matchers {
  "Code" should "set stack size to 1 for load operators" in {
    Seq(Op.iconst(1), Op.bipush(1)).foreach { op =>
      val code = Code.op(op)
      code.stackSize should be(1)
      code.maxStackSize should be(1)
    }
  }

  "Code" should "increase stack stackSize if multiple loads are done" in {
    val code = Code.empty + Op.iconst(1) + Op.bipush(1)

    code.stackSize should be(2)
    code.maxStackSize should be(2)
  }

  "Code" should "decrease end stack size if pops added, but keep max stack size" in {
    val code = Code.empty +
      Op.iconst(1) +
      Op.bipush(1) +
      Op.iadd

    code.stackSize should be(1)
    code.maxStackSize should be(2)
  }
}
