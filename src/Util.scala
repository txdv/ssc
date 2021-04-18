package lt.vu.mif.bentkus.bachelor.compiler

object Util {
  def check[A, B](state: List[(A, List[B])]): Option[A] = {
    state
      .find { case (_, tokens) => tokens.isEmpty }
      .map { case (tree, tokens) => tree }
  }

  def error[A, B](state: List[(A, List[B])]): Option[A] = {
    val result = check(state)

    if (result.isEmpty) {
      println("Failed parsing:")
      state.zipWithIndex.foreach { case ((tree, tokens), i) =>
        println(s"$i.")
        println(s"\ttree: ${tree}")
        println(s"\ttokens: ${tokens.mkString(",")}")
      }
    }

    result
  }
}
