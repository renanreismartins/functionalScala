import scala.util.Try
//import Chap03._

object Chap04 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(v) => Some(f(v))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(v) => v
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(v) => Some(v)
      case None => ob
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(v) => if (f(v)) Some(v) else None
      case None => None
    }
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(e => math.pow(e - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(va => b.map(vb => f(va, vb)))

  //  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  //    def loop(as: List[Option[A]], acc: List[A]): List[A] = {
  //      as match {
  //        case Cons(Some(v), xs) => loop(xs, Cons(v, acc))
  //        case Cons(None, _) => Nil
  //        case Nil => acc
  //      }
  //    }
  //
  //    loop(a, Nil) match {
  //      case Nil => None
  //      case x => Some(x)
  //    }
  //  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def sequenceRight[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((opA, optionListA) => map2(opA, optionListA)((opaValue, listA) => opaValue :: listA))

  def traverseInTermsOfSequence[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a map (i => f(i)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h) flatMap (b => traverse(t)(f) map (b :: _))
    }


  def main(args: Array[String]): Unit = {

    println("traverseInTermsOfSequence")
    println(Chap04.traverseInTermsOfSequence(List(Some("1"), Some("2")))(i => Some(i)))
    println()

    println("Map")
    println(Some("Renan").map(_.toUpperCase))
    println(None.map(a => a))
    println()

    println("flatMap")
    println(Some("Renan").flatMap(v => Some(v.toUpperCase)))
    println(None.flatMap(a => Some(a)))
    println()

    println("getOrElse")
    println(Some("Renan").getOrElse(() => "no value"))
    println(None.getOrElse(() => "no value")())
    println()

    println("orElse")
    println(Some("Renan").orElse(Some("no value")))
    println(None.orElse(Some("no value")))
    println()

    println("filter")
    println(Some("Renan").filter(_.length > 3))
    println(Some("Renan").filter(_.length > 10))
    println(None.filter(_ => true))
    println()

    //    println("sequence")
    //    println(Chap04.sequence(Cons(Some("Renan"), Cons(Some("Reis"), Nil))))
    //    println(Chap04.sequence(Cons(Some("Renan"), Cons(Some("Reis"), Cons(None, Nil)))))
    //    println()

    println("sequence")
    println(Chap04.sequence(List(Some("a"), None, Some("b"))))
    println(Chap04.sequence(List(None)))
  }
}