package lt.vu.mif.bentkus.bachelor.compiler.classfile

object CodeAttributeApp extends App {
  val result = Instr.parse(Array[Byte](0xB1.toByte))
  println(s"result: $result")
}
