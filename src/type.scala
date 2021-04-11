package lt.vu.mif.bentkus.bachelor.compiler.classfile.types.runtime

import lt.vu.mif.bentkus.bachelor.compiler.classfile.higher.JavaType
import lt.vu.mif.bentkus.bachelor.compiler.classfile.higher.MethodRef

case class ClassObj(jclass: JavaType.Class, methods: Seq[MethodRef])

object Types {
  private def convert(javaClass: Class[_]): JavaType = {
    javaClass.getName match {
      case "int" =>
        JavaType.Int
      case "long" =>
        JavaType.Long
      case "float" =>
        JavaType.Float
      case "double" =>
        JavaType.Double
      case "void" =>
        JavaType.Void
      case "boolean" =>
        JavaType.Boolean
      case "java.lang.String" =>
        JavaType.String
      case className =>
        JavaType.Class(className.replace(".", "/"))
    }
  }

  def resolve(path: String): ClassObj = {
    val classz = Class.forName(path)
    val jclass = JavaType.Class(classz.getName.replace(".", "/"))
    val methods = classz.getMethods.map { method =>
      MethodRef(
        jclass,
        method.getName,
        Seq(convert(method.getReturnType)) ++
          method.getParameterTypes.toSeq.map(convert)
      )
    }
    ClassObj(jclass, methods)
  }

  def main(args: Array[String]): Unit = {
    import lt.vu.mif.bentkus.bachelor.compiler.misc.PrettyPrint
    val classes = if (args.size == 0) Seq("java.lang.Math") else args.toSeq

    classes.foreach { name =>
      PrettyPrint.pformat(resolve(name))
    }
  }
}
