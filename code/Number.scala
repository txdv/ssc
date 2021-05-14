val number: Parser[Expr] = for {
  number <- token(sat[Number])
} yield Num(number.value)
