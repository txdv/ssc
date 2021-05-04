sealed trait Expression
object Expression {
  case class Int32(value: Int) extends Epression
  case class Sum(left: Expression, right: Expression) extends Expression
  case class Multiply(left: Expression, right: Expression) extends Expression
}
