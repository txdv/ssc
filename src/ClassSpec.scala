package lt.vu.mif.bentkus.bachelor.compiler.classfile.higher

import org.scalatest._
import flatspec._
import matchers._

class ClassSpec extends AnyFlatSpec with should.Matchers {
  import JavaType._

  "from" should "should parse correctly" in {
    from("I") should be (Int)
    from("C") should be (Char)
    from("V") should be (Void)
    from("[I") should be (Array(Int))
    from("Ljava/lang/String;") should be (Class("java/lang/String"))
    from("[Ljava/lang/String;") should be (Array(Class("java/lang/String")))

  }

  "value representation of JavaTypes" should "be correct" in {
    Int.value should be ("I")
    Char.value should be ("C")
    Void.value should be ("V")
    Array(Int).value should be ("[I")
    Class("java/lang/String").value should be ("Ljava/lang/String;")
    Array(Class("java/lang/String")).value should be ("[Ljava/lang/String;")
  }

  "parse" should "parse something" in {
    parse("([Ljava/lang/String;)V") should be (Seq(Array(Class("java/lang/String")), Void))
    parse("(CI)V") should be (Seq(Char, Int, Void))
  }

  "JavaType" should "parse main method descriptor string" in {
    val args = "([Ljava/lang/String;)V"
    val expected = Seq(Array(Class("java/lang/String")), Void)
    JavaType.parse(args) should be (expected)
  }

  def arrayFrom(str: String): Array = {
    from(str).asInstanceOf[Array]
  }

  "Array" should "calculate correct arity" in {
    arrayFrom("[I").arity should be (1)
    arrayFrom("[[I").arity should be (2)
  }

}
