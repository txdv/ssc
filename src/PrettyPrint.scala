package ssc.misc

case class Field(name: String, value: Value)

sealed trait Value {
  val depth: Int

}
case class SimpleValue(typeName: String, value: Any, fields: Seq[Field]) extends Value {
  val depth: Int = {
    if (fields.isEmpty) {
      1
    } else {
      1 + fields.map(_.value.depth).max
    }
  }
}
case class CollectionValue(typeName: String, values: Seq[Value]) extends Value {
  val depth: Int = {
    if (values.isEmpty) {
      0
    } else {
      values.map(_.depth).max
    }
  }
}

object PrettyPrint {
  // inspired by https://stackoverflow.com/questions/15718506/scala-how-to-print-case-classes-like-pretty-printed-tree

  def pformat(obj: Any): Unit = {
    println(format(meta(obj)))
  }


  def meta(obj: Any): Value = {
    val prettyName = if (obj == null) "null" else obj.getClass.getSimpleName

    obj match {
      case seq: Array[_] =>
        CollectionValue("Array", seq.toSeq.map(item => meta(item)))
      case seq: Iterable[_] =>
        CollectionValue("Iterator", seq.toSeq.map(item => meta(item)))
      case obj: Product =>
        val fields = (obj.productIterator zip obj.productElementNames).map {
          case (subObj, paramName) => Field(paramName, meta(subObj))
        }.toSeq
        SimpleValue(prettyName, obj, fields)
      case _ =>
        SimpleValue(prettyName, obj, Seq.empty)
    }
  }

  private val tab = "  "

  def format(value: Value)(implicit depth: Int = 0): String = {
    val indent = tab * depth
    val indent1 = tab * (depth + 1)

    value match {
      case simple: SimpleValue =>
        if (simple.fields.nonEmpty) {
          val indent = tab * depth
          val indent1 = tab * (depth + 1)

          if (simple.depth <= 2 && simple.fields.size <= 5) {
            simple.typeName + "(" + simple.fields.map { field =>
              val nested = format(field.value)(depth + 1)
              s"${field.name} = ${nested}"
            }.mkString(", ") + ")"
          } else {
            val fields = simple.fields.map { field =>
              val nested = format(field.value)(depth + 1)
              s"${field.name} = ${nested}"
            }.mkString(",\n" + indent1)
            simple.typeName + "(\n" + indent1 + fields + s"\n$indent)"
          }
        } else {
          if (simple.value == null) {
            "null"
          } else {
            simple.value.toString
          }
        }
      case collection: CollectionValue =>
        if (collection.depth <= 2 && collection.values.size <= 5) {
          "[" + collection.values.map(value => format(value)(depth + 1)).mkString(", ") + "]"
        } else {
          "[\n" +
            collection.values.foldLeft("") { case (str, value) =>
              str + indent1 + format(value)(depth + 1) + ",\n"
            } +
            indent + "]"
        }
    }
  }
}



object MainApp extends App {
  case class Test(a: Int, b: Int)
  case class Test2(c: Int, t: Test)
  case class Test3(t: Test2, a: Seq[Int])

  val tests1 = Seq(Test(1, 2), Test(2, 3))

  val t1 = Test(a = 1, b = 2)
  val t2 = Test2(c = 3, t1)
  val t3 = Test3(t2, Seq(1, 2, 3))

  import PrettyPrint._

  pformat(Seq(1, 2, 3, 4))
  pformat(Seq(1, 2, 3, 4, 5, 6))
  pformat(Array(1, 2, 3, 4))
  pformat(1)
  pformat("ASD")
  pformat(t1)
  pformat(t2)
  pformat(t3)

  pformat(tests1)

  pformat(Test3(t2, Seq(1, 2, 3, 4, 5, 6)))
}
