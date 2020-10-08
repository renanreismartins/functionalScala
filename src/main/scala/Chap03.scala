import scala.annotation.tailrec

object Chap03 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    //[] = 0
    //[1] 1 + sum([])
    // 1 + 0
    // 1
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def sumTail(ints: List[Int]): Int = {
      @tailrec
      def loop(l: List[Int], acc: Int): Int = l match {
        case Nil => acc
        case Cons(x, xs) => loop(xs, acc + x)
      }

      loop(ints, 0)
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

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }

    // this gives a reversed init list.
    // maybe count the number of elemtns then use drop[A](l: List[A], n: Int)?
    def init[A](l: List[A]): List[A] = {
      def loop(l: List[A], acc: List[A]): List[A] = {
        l match {
          case Cons(_, Nil) => acc
          case Cons(x, xs) => loop(xs, Cons(x, acc))
          case Nil => acc
        }
      }

      loop(l, Nil)
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

   def length[A](as: List[A]): Int = foldRight(as, 0)((_: A, acc: Int) => acc + 1)

    def sumFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    def productFoldLeft(ds: List[Double]): Double = foldLeft(ds, 1D)(_ * _)

    def lengthFoldLeft[A](as: List[A]): Int = foldLeft(as, 0)((acc: Int, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((acc: List[A], a: A) => Cons(a, acc))

    def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)((h, acc) => Cons(h, acc))

    def addOneToListOfInts(l: List[Int]): List[Int] = l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, addOneToListOfInts(xs))
    }

    def transformDoubleIntoString(l: List[Double]): List[String] = l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x.toString, transformDoubleIntoString(xs))
    }

    def map[A,B](as: List[A])(f: A => B): List[B] = as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
    }

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
      case Nil => Nil
      case Cons(x, xs) => appendViaFoldRight(f(x), flatMap(xs)(f))
    }

    def filterFromFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(e => if (f(e)) List(e) else List())

    def addCorrespondingItems(l: List[Int], r: List[Int]): List[Int] = l match {
      case Nil => Nil
      case Cons(x, xs) => r match {
        case Nil => Nil
        case Cons(y, ys) => Cons(x + y, addCorrespondingItems(xs, ys))
      }
    }

    def zipWith[A, B](l: List[A], r: List[A])(f: (A, A) => B): List[B] = l match {
      case Nil => Nil
      case Cons(x, xs) => r match {
        case Nil => Nil
        case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
      }
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      def loop(sup: List[A], sub: List[A], r: Boolean): Boolean = {
        sup match {
          case Nil => sub == Nil
          case Cons(x, xs) => sub match {
            case Nil => r
            case Cons(y, ys) => if (x == y) loop(xs, ys, true) else loop(sup, ys, false)
          }
        }
      }

      loop(sup, sub, false)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List.x)
    println(List.tail(List(1, 2, 3, 4, 5)))
    println(List.drop(List(1, 2, 3, 4, 5), 2))
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3))
    println(List.init(List()))
    println(List.length(List(1, 2, 3)))
    println(List.foldLeft(List(1, 2, 3), 0)(_ + _))
    println(List.sumFoldLeft(List(1, 2, 3)))
    println(List.productFoldLeft(List(1, 2, 3)))
    println(List.lengthFoldLeft(List(1, 2, 3)))
    println(List.reverse(List(1, 2, 3)))
    println(List.appendViaFoldRight(List(1, 2, 3), List(4, 5, 6)))
    println(List.addOneToListOfInts(List(1, 2, 3)))
    println(List.transformDoubleIntoString(List(1, 2, 3)))
    println(List.map(List(1, 2, 3))(_ + 1))
    println(List.filter(List(1, 2, 3))(_ % 2 == 0))
    println(List.flatMap(List(1,2,3))(i => List(i,i)))
    println(List.filterFromFlatMap(List(1, 2, 3))(_ % 2 == 0))
    println(List.addCorrespondingItems(List(1, 2, 3), List(4,5,6)))
    println(List.zipWith(List(1, 2, 3), List(4,5,6))(_ + _))
    println(List.hasSubsequence(List(1, 2, 3), List(1, 3)))
    println(List.hasSubsequence(List(1, 2, 3), List(1, 2)))
    println(List.hasSubsequence(List(1, 2, 3), List(5)))
    println(List.hasSubsequence(List(1), List(5)))

//    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
//      case Nil => z
//      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
//    }
    //println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    println(List.foldRight(List(1,2,3), Nil:List[Int])((a, b) => Cons(a, b)))
    println(List.foldRight(List(1,2,3), 0)((a, b) => a + b))
    println(List.sumTail(List(1,2,3)))
  }

}
