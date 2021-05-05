import org.specs2.mock.Mockito
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope

class SampleTest extends SpecificationWithJUnit with Mockito {
  "test group" should {
    "first test" in new Context {
      one must_=== 1
    }

    "second test" in new Context {
      (one + one) must_=== 2
    }
  }

  trait Context extends Scope {
    val one = 1
  }
}
