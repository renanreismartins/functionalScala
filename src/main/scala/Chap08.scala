import Chap05.Stream
import java.util.concurrent.{ExecutorService, Executors}

import Chap08.{Gen, Prop}

object Chap08 {


  /*
  The library developed in this chapter goes through several iterations. This file is just the
  shell, which you can fill in and modify while working through the chapter.
  */

  trait Prop {
    def check: Boolean
    def &&(p: Prop): Prop = if (this.check && p.check) Success else Failure
  }

  object Prop {
    def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
  }

  case object Success extends Prop {
    override def check: Boolean = true
  }

  case object Failure extends Prop {
    override def check: Boolean = false
  }

  object Gen {
    def unit[A](a: => A): Gen[A] = ???
  }

  trait Gen[A] {
    def map[A, B](f: A => B): Gen[B] = ???

    def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
  }

  trait SGen[+A] {

  }

}
