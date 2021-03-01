package lt.vu.mif.bentkus.bachelor.compiler.classfile.higher

import org.scalatest._
import flatspec._
import matchers._

class ClassSpec extends AnyFlatSpec with should.Matchers {
  import JavaType._

  "JavaType" should "parse main method descriptor string" in {
    val args = "([Ljava/lang/String;)V"
    val expected = Seq(Array(Class("java/a/String")), Void)
    JavaType.parse(args) should be (expected)
  }
}
