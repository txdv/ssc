package lt.vu.mif.bentkus.bachelor.compiler.classfile

import lt.vu.mif.bentkus.bachelor.compiler.classfile.higher.MethodAccessFlag
import Constant._

import java.io.File
import java.nio.file.Files
import java.time.ZoneId
import java.time.format.DateTimeFormatter

object ClassFileApp extends App {

  val formatter = DateTimeFormatter.ofPattern("LLL d, YYYY")
    .withZone(ZoneId.systemDefault())

  def toHex(i: Int): String = {
    String.format("0x%04X", i)
  }

  def repeat(char: Char, times: Int): String = {
    (1 to times).map(_ => char).mkString("")
  }

  def leftpad(str: String, filler: Char = ' ', size: Int = 5): String = {
    val i = Math.max(size - str.length, 0)
    repeat(filler, i) + str
  }

  def rightpad(str: String, filler: Char = ' ', size: Int = 19): String = {
    val i = Math.max(size - str.length, 0)
    str + repeat(filler, i)
  }

  def toValue(c: Constant): String = {

    c match {
      case MethodRef(klass, nameType) =>
        s"#$klass.#$nameType"
      case Class(i) =>
        s"#$i"
      case NameAndType(index, descriptor) =>
        s"#$index:#$descriptor"
      case Utf8(str) =>
        str
      case _ =>
        c.toString
    }
  }

  def toComment(classFile: ClassFile, constant: Constant): Option[String] = {
    import classFile._

    constant match {
      case MethodRef(klass, nameAndTypeId) =>
        val b = nameAndType(nameAndTypeId)
        val value = s"${className(klass)}.${b}"
        Some("// ").map(_ + value)
        //Some(s"// $value")
      case Class(index) =>
        val value = string(index)
        Some("// ").map(_ + value)
      case n: NameAndType =>
        val value = nameAndType(n)
        Some("// ").map(_ + value)
      case _ =>
        None
    }
  }

  def keywords(flags: Set[MethodAccessFlag]): String = {
    flags.toSeq.
      sortBy(_.value)
      .map(_.toString.toLowerCase)
      .mkString(" ")
  }

  def print(classFile: ClassFile): Unit = {
    import classFile._
    val thisClassString = className(thisClass)
    println(s"public class $thisClassString")
    println(s"  minor version: ${version.minor}")
    println(s"  major version: ${version.major}")
    println(s"  flags: (${toHex(accessFlags)})")
    println(s"  this_class: #${thisClass} // $thisClassString")
    println(s"  super_class: #${superClass} // ${className(superClass)} ")
    println(s"  interfaces: 0, fields: 0, methods: ${methods.size}, attributes: 0")
    println("Constant pool:")
    constants.zipWithIndex.foreach { case (constant, i) =>
      val nr = i + 1
      constant match {
        case _ =>
          val nrstr = leftpad(s"#$nr", ' ')
          val name = rightpad(constant.getClass.getSimpleName, ' ', 18)
          val value = rightpad(toValue(constant), ' ', 15)
          val comments = toComment(classFile, constant).getOrElse("")
          println(s"$nrstr = $name $value $comments")
      }
    }
    println("{")
    methods.foreach { method =>
      val methodName = string(method.name) match {
        case "<init>" => thisClassString
        case str => str
      }

      val flags = MethodAccessFlag.parse(method.accessFlags)
      val kw = keywords(flags)
      val descriptorName = string(method.descriptor)
      println(s"  $kw $methodName $descriptorName")
      println(s"    descriptor: $descriptorName")

      val stringFlags = flags.toSeq.sortBy(_.value).map(s => s"ACC_${s.toString.toUpperCase}").mkString(", ")
      println(s"    flags: (${toHex(method.accessFlags)}) $stringFlags")
      println
    }

    println("}")
  }

  args.headOption.map { filename =>
    val file = new File(filename)
    val bytes = Files.readAllBytes(file.toPath)

    import java.nio.file.attribute.BasicFileAttributes

    val t = Files.readAttributes[BasicFileAttributes](file.toPath, classOf[BasicFileAttributes])
    val cf = ClassFile.parse(bytes)

    val date = formatter.format(t.lastModifiedTime.toInstant)
    println(s"  Last modified $date; size ${bytes.size} bytes")

    print(cf)
  } getOrElse {
    println("Please pass file.")
  }
}
