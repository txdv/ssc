object LazyListExtensions {
  implicit class WhileLast[A](stream: LazyList[A]) {
    def takeWhileLast(p: A => Boolean): LazyList[A] =
      if (stream.nonEmpty)
        if (p(stream.head)) LazyList.cons(stream.head, stream.tail.takeWhileLast(p))
        else LazyList.cons(stream.head, LazyList.empty)
      else LazyList.empty
  }
}
