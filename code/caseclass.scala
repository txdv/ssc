sealed trait Expression
case class Int32(value: Int) extends Expression
case class Sum(left: Expression, right: Expression) extends Expression
case class Multiply(left: Expression, right: Expression) extends Expression
