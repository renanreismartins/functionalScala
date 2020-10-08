package chap02

import scala.annotation.tailrec

object Chap02 {


  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f.apply(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  //[1, 2]
  //[2]
  @tailrec
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if (as.isEmpty) true
    else if (as.length == 1) true
    else if (ordered(as(0), as(1))) isSorted(as.drop(1), ordered) else false
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1,2), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1,2,3), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1,3,1), (a: Int, b: Int) => a < b))
    println(isSorted(Array(2,3,1), (a: Int, b: Int) => a < b))
    //println(isSorted(Array(1,2), (a: Int, b: Int) => a < b))
  }
}