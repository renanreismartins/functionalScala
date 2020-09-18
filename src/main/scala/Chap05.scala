object Chap05 {

  sealed trait Stream[+A] {
    def toList: List[A] = {
      def loop(s: Stream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(h, t) => loop(t(), h() :: acc)
      }

      loop(this, Nil)
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

  }

  def main(args: Array[String]): Unit = {
    println("toList")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).toList)
    println()
  }
}
