import org.specs2.matcher.Matcher
import org.specs2.mock.Mockito
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope

class SampleTest extends SpecificationWithJUnit with Mockito {
  "group" should {
    "test1" in new Context {
      1 must_=== 2
    }

    "test2" in new Context {
      1 must_=== 2
    }
  }

  trait Context extends Scope {
  }
}
