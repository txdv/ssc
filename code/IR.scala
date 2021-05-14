sealed trait AST

object AST {
  case class DefObject(name: String, statements: Seq[Statement] = Seq.empty) extends Statement

  sealed trait Statement extends AST

  sealed trait ScalaType extends Expression
  case class SimpleType(name: String) extends ScalaType
  case class GenericType(name: String, generics: Seq[ScalaType]) extends ScalaType

  case class DefMethod(
    name: String,
    returnType: ScalaType,
    arguments: Seq[DefMethodArgument] = Seq.empty,
    body: Option[Expr] = None) extends Statement

  case class DefMethodArgument(name: String, argumentType: ScalaType)

  sealed trait Expr extends Statement {
    val depth: Int
  }

  // ...
}
