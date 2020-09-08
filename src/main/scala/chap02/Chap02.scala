package chap02

object Chap02 {
  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) true
    else if (ordered(as(0), as(1))) isSorted(as.tail, ordered)
    else false
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f.apply(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1,2), (a: Int, b: Int) => a < b))
  }
}