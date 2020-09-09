object Chap03 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    def tail[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    def setHead[A](as: List[A], a: A): List[A] =
      as match {
        case Nil => Cons(a, Nil) // cons or error?
        case Cons(_, xs) => Cons(a, xs)
      }

    def drop[A](l: List[A], n: Int): List[A] =
      if (n == 0) l
      else if (l == Nil) Nil
      else drop(List.tail(l), n - 1)

  }

  def main(args: Array[String]): Unit = {
    println(List.x)
    println(List.tail(List(1, 2, 3, 4, 5)))
    println(List.drop(List(1, 2, 3, 4, 5), 2))
  }

}