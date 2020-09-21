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
      case Cons(_, t) => if (n == 0) this else t().drop(n - 1)
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      def loop(s: Stream[A], acc: Stream[A], p: A => Boolean): Stream[A] = s match {
        case Empty => Empty
        case Cons(h, t) => if (p(h())) cons(h(), loop(t(), acc, p)) else acc
      }

      loop(this, Empty, p)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)
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

    println("take while")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).takeWhile(_ == 1).toList)
    println()

    println("for all")
    println(Stream.cons(1, Stream.cons(1, Stream.empty)).forAll(_ == 1))
    println()
  }
}
