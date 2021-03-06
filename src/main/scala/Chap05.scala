import Chap05.Stream.{cons, unfold}

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

    def takeWhileInTermsOfFoldRight(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

    def headOptionInTermsOfFoldRight(): Option[A] = foldRight(None: Option[A])((a, _) => if (true) Some(a) else None)

    def map[B](f: A => B): Stream[B] = this match {
      case Empty => Empty
      case Cons(h, t) => cons(f(h()), t().map(f))
    }

    def mapInTermsOfFoldRight[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((e, acc) => cons(f(e), acc))

    def filterInTermsOfFoldRight(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((e, acc) => if (f(e)) cons(e, acc) else acc)

    def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((e, acc) => acc.append(f(e)))

    def mapUnfold[B](f: A => B): Stream[B] = unfold(this)(s => s match {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    })

    def takeUnfold(n: Int): Stream[A] = unfold((this, n)) {
      case (Empty, _) => None
      case (Cons(h, t), x) => if (x <= n) Some(h(), (t(), x + 1)) else None
    }

    def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this) {
      case Empty => None
      case Cons(h, t) => if (p(h())) Some(h(), t()) else None
    }

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h, t), Cons(h2, t2)) => Some(f(h(), h2()), (t(), t2()))
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Empty) => None
      case (Cons(h, t), Cons(h2, t2)) => Some((Some(h()), Some(h2())), (t(), t2()))
    }

    def tails: Stream[Stream[A]] = unfold(this) {
      case Empty => None
      case s => Some(s, s drop 1)
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

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case None => Empty
      case Some((e, s)) => cons(e, unfold(s)(f))
    }

    def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s + 1, s + 1))

    def constantUnfold[A](a: A): Stream[A] = unfold(a)(s => Some(a, s))


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

    println("take while in terms of foldRight")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).takeWhileInTermsOfFoldRight(_ == 1).toList)
    println()

    println("headOption terms of foldRight")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).headOptionInTermsOfFoldRight())
    println(Stream.empty.headOptionInTermsOfFoldRight())

    println("map")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).map(_ - 1).toList)
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).mapInTermsOfFoldRight(_ - 1).toList)

    println("filter")
    println(Stream.cons(1, Stream.cons(2, cons(3, Stream.empty))).filterInTermsOfFoldRight(_ == 2).toList)

    println("flatMap")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).flatMap(a => cons(a + 1, cons(a + 2, Stream.empty))).toList)

    println("constant")
    println(Stream.constant(1).take(4).toList)

    println("from")
    println(Stream.from(4).take(4).toList)

    println("unfold")
    println(Stream.unfold(0)(s => Some(s + 1, s + 1)).take(4).toList)

    println("from in terms of unfold")
    println(Stream.fromUnfold(4).take(3).toList)

    println("constant in terms of unfold")
    println(Stream.constant(4).take(4).toList)

    println("map in terms of unfold")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).mapUnfold(_ + 1).toList)

    println("take in terms of unfold")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).takeUnfold(1).toList)

    println("take while in terms of unfold")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).takeWhileUnfold(_ == 1).toList)
    println()

    println("zipWith in terms of unfold")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).zipWith(Stream.cons(1, Stream.cons(2, Stream.empty)))((a, b) => a + b).toList)
    println()

    println("zipAll in terms of unfold")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).zipAll(Stream.cons(1, Stream.cons(2, Stream.empty))).toList)
    println()

    println("tails in terms of unfold")
    println(Stream.cons(1, Stream.cons(2, Stream.empty)).tails.toList)
    println()
  }
}