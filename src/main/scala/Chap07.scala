import java.util.concurrent._

import Chap07.Par.{Par, fork}

object Chap07 {

  object Par {
    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sequence[A](l: List[Par[A]]): Par[List[A]] =
      l.foldRight(unit(List[A]()))((h, t) => map2(h, t)(_ :: _))

    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = ???

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get


  }

//  def sum2(ints: IndexedSeq[Int]): Int =
//    if (ints.size <= 1)
//      ints.headOption getOrElse 0
//    else {
//      val (l, r) = ints.splitAt(ints.length / 2)
//      sum(l) + sum(r)
//    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }


  def main(args: Array[String]): Unit = {
    println(Par.run(Executors.newFixedThreadPool(10))(sum(IndexedSeq(1, 2, 3))))
  }

}