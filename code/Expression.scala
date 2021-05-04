sealed trait Expression
class Int32(val value: Int) extends Expression
class Sum(val left: Expression, val right: Expression) extends Expression
class Multiply(val left: Expression, val right: Expression) extends Expression
