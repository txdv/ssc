package lt.vu.mif.bentkus.bachelor.compiler.classfile.higher

import org.scalatest._
import flatspec._
import matchers._

class ClassSpec extends AnyFlatSpec with should.Matchers {
  import JavaType._

  "a" should "b" in {
    val args = "([Ljava/lang/String;)V"
    val expected = Seq(Arr(JavaClass("java/a/String")), JavaVoid)
    JavaType.parse(args) should be (expected)
  }
}
