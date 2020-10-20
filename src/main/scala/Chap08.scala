import Chap05.Stream
import java.util.concurrent.{ExecutorService, Executors}

import Chap08.{Gen, Prop}
import RNG.Simple

object Chap08 {


  /*
  The library developed in this chapter goes through several iterations. This file is just the
  shell, which you can fill in and modify while working through the chapter.
  */

  trait Prop {
    def check: Boolean

    def &&(p: Prop): Prop = new Prop {
      def check = Prop.this.check && p.check
    }

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
    }
  }

  object Prop {
    def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
  }

  case class Gen[A](sample: State[RNG, A])

  object Gen {
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(n => n % 2 == 0))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val a: Seq[State[RNG, A]] = List.fill(n)(g.sample)
      val b: State[RNG, Seq[A]] = State.sequence(a)
      Gen(b)
    }
  }

//  trait Gen[A] {
//
//    def map[A, B](f: A => B): Gen[B] = ???
//
//    def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
//  }

  trait SGen[+A] {

  }

}
