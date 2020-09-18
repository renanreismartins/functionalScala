import Chap05.Stream.cons

object Chap05 {

  sealed trait Stream[+A] {
    def toList: List[A] = {
      def loop(s: Stream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(h, t) => loop(t(), h() :: acc)
      }

      loop(this, Nil).reverse
    }

    def take(n: Int): Stream[A] = {
      def loop(s: Stream[A], acc: Stream[A], i: Int): Stream[A] = s match {
        case Empty => acc
        case Cons(h, t) => if (i > 0) cons(h(), loop(t(), acc, i - 1)) else acc
      }

      loop(this, Empty, n)
    }

    def drop(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(_, t) => if (n == 0) this else t().drop(n -1)
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

    println("take")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).take(2).toList)
    println()

    println("drop")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).drop(1).toList)
    println()
  }
}
