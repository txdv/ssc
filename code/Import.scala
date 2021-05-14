val identifier = token(sat[Identifier])
val fullIdentifier = sepBy1(identifier, `.`)
val `import`: Parser[Import] = for {
  _ <- identifierWithName("import")
  name <- fullIdentifier
  fullname = name.map(_.value).mkString(".")
} yield Import(fullname)
