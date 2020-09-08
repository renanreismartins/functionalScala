package chap02

object Chap02 {
  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) true
    else if (ordered(as(0), as(1))) isSorted(as.tail, ordered)
    else false
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1,2), (a: Int, b: Int) => a < b))
  }
}