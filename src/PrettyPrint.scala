package lt.vu.mif.bentkus.bachelor.compiler.misc

object PrettyPrint {
  // https://stackoverflow.com/questions/15718506/scala-how-to-print-case-classes-like-pretty-printed-tree
  def pformat(obj: Any, depth: Int = 0, paramName: Option[String] = None): Unit = {

    val indent = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    val ptype = obj match {
      case _: Iterable[Any] => ""
      case obj: Product => obj.productPrefix
      case null => "null"
      case _ => obj.toString
    }

    println(s"$indent$prettyName$ptype")

    obj match {
      case seq: Iterable[Any] =>
        seq.foreach(pformat(_, depth + 1))
      case obj: Product =>
        (obj.productIterator zip obj.productElementNames)
          .foreach { case (subObj, paramName) => pformat(subObj, depth + 1, Some(paramName)) }
      case _ =>
    }
  }
}


object MainApp extends App {
  case class Test(a: Int, b: Int)
  case class Test2(c: Int, t: Test)

  val t1 = Test(a = 1, b = 2)
  val t2 = Test2(c = 3, t1)

  import PrettyPrint._

  pformat(Seq(1, 2, 3, 4))
  pformat(Array(1, 2, 3, 4))
  pformat(1)
  pformat("ASD")
  pformat(t1)
  pformat(t2)
}
