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
      // TODO: calculate stack automatically
      // code.currentStack should be(Seq(StackElement(JavaType.Int)))
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

  "Code" should "take into account invoke method argument count" in {
    // first element in the signature is the return type
    val invokeWith2Args = Code.empty + Op.invoke(
      MethodRef(JavaType.Object, "method", Seq(JavaType.Int, JavaType.Int, JavaType.Int)),
      Op.invoke.virtual,
    )

    val invokeWith1Args = Code.empty + Op.invoke(
      MethodRef(JavaType.Object, "method", Seq(JavaType.Int, JavaType.Int)),
      Op.invoke.virtual,
    )

    invokeWith1Args.stackSize should be(0)
  }

  "Code" should "represent negative stacks" in {
    Code.op(Op.iadd).stackSize shouldBe -1
  }

  "Code" should "test" in {
    val op = Seq(Op.bipush(2))
    Code.calculateStack(op) shouldBe Seq(StackElement(JavaType.Int))
  }

  "Code" should "multiple bipush" in {
    val op = Seq(Op.bipush(2), Op.bipush(3))
    Code.calculateStack(op) shouldBe Seq(StackElement(JavaType.Int), StackElement(JavaType.Int))
  }

  "Code" should "handle aload" in {
    Code.calculateStack(Seq(Op.aload(0)), Seq(JavaType.Int)) shouldBe Seq(StackElement(JavaType.Int))
  }

  "code" should "handle newobj" in {
    Code.calculateStack(Seq(Op.newobj(JavaType.Object))) shouldBe Seq(StackElement(JavaType.Object))
  }
}

