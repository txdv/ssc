package lt.vu.mif.bentkus.bachelor.compiler.misc

import scala.reflect.ClassTag

case class Field(name: String, value: Value)
case class Value(name: String, value: Any, fields: Seq[Field])

object PrettyPrint {
  import reflect.runtime.universe._

  // https://stackoverflow.com/questions/7525142/how-to-programmatically-determine-if-the-class-is-a-case-class-or-a-simple-class
  def isCaseClass[T](o: T) = {
    o.getClass.getInterfaces.contains(classOf[scala.Product])
  }

  def meta[T](obj: T)(implicit typeTag: TypeTag[T]): Value = {
    meta2(obj, typeTag.tpe)
  }

  val noArgs = new Array[Object](0)

  def meta2[T](obj: T, `type`: Type): Value = {
    val name = obj.getClass.getSimpleName

    val fields =
      if (isCaseClass(obj)) {
        val accessors = `type`.members.collect {
          case m: MethodSymbol if m.isCaseAccessor => m
        }

        accessors.map { accessor =>
          val method = obj.getClass.getMethod(accessor.name.toString)

          val value = method.invoke(obj, noArgs:_*)


          Field(accessor.name.toString, meta2(value, accessor.typeSignature))
        }.toSeq
      } else {
        Seq.empty
      }

    Value(name, obj, fields)
  }


  private val tab = "  "

  def pformat[T](obj: T)(implicit typeTag: TypeTag[T]): Unit = {
    println(format(meta(obj)))

  }

  def format(value: Value)(implicit depth: Int = 0): String = {
    if (value.fields.nonEmpty) {
      val indent = tab * depth
      val indent1 = tab * (depth + 1)

      val fields = value.fields.map { field =>
        val nested = format(field.value)(depth + 1)
        s"${field.name} = ${nested}"
      }.mkString(",\n" + indent1)


      value.name + "(\n" + indent1 + fields + s"\n$indent)"
    } else {
      value.value.toString
    }
  }
}


object MainApp extends App {
  case class Test(a: Int, b: Int)
  case class Test2(c: Int, t: Test)

  val t1 = Test(a = 1, b = 2)
  val t2 = Test2(c = 3, t1)

  import PrettyPrint._

  pformat(1)
  pformat("ASD")
  pformat(t1)
  pformat(t2)
}
