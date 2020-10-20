trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  type Rand[+A] = RNG => (A, RNG)

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return va/src/main/scala/Chap04.scalaalue is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i < 0) (-(i + 1), r) else (i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r2) = intDouble(rng)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(counta: Int, rnga: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (counta == 0) (acc, rnga)
      else {
        val (i, s) = rnga.nextInt
        loop(counta - 1, s, i :: acc)
      }
    }

    loop(count, rng, List.empty)
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[S, A, B](s: S => (A, S))(f: A => B): S => (B, S) =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  val _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    val x: Rand[B] = g(a)
    val z: (B, RNG) = x(r)
    z
  }

  def mapInTermsOfFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = RNG.flatMap(s)(a => unit(f(a)))

  //def map2InTermsOfFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = RNG.flatMap(ra)(a => RNG.flatMap(rb)(b => unit(f(a, b))))
  def map2InTermsOfFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = RNG.flatMap(ra)(a => RNG.map(rb)(b => f(a, b)))

  def main(args: Array[String]): Unit = {
    println("non negative")
    println(RNG.nonNegativeInt(Simple(1L)))

    println("ints")
    println(RNG.ints(3)(Simple(1L)))

    println("double")
    println(RNG.double(Simple(1L)))

    println("int")
    println(RNG.int(Simple(1L)))

    println("unit")
    println(RNG.unit(1L)(Simple(1L)))

    println("map")
    val a: RNG => (Int, RNG) = RNG.map(RNG.nonNegativeInt)(_ + 1)
    println(RNG.map(RNG.nonNegativeInt)(_ + 1)(Simple(1L)))

    println("double in terms of map")
    println(RNG._double(Simple(1L)))

    println("map2")
    println(RNG.map2(RNG.int, RNG.int)((a, b) => a + b)(Simple(1L)))
  }

}

import State.{modify, _}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map_[B](f: A => B): State[S, B] = {
    ???
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: Seq[State[S, A]]): State[S, Seq[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))


  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def map[S, A, B](fa: State[S, A])(f: A => B): State[S, B] = State[S, B] { s: S =>
    val (a, s2) = fa.run(s)
    val b: B = f(a)
    (b, s2)
  }

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

  def main(args: Array[String]): Unit = {
    //    val m: Machine = Machine(true, 5, 10)
    //
    //
    //
    //    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    //    val ab: State[Machine, (Int, Int)] = Candy.simulateMachine(inputs)
    //    println(Candy.simulateMachine(inputs).run(m)._1)


    get[Int].run(3)._1 == 3
    get[Int].map(_ + 1)
    val a: State[Int, Int] = get[Int]
    val b: (Int, Int) = a.run(3)
    //val b: (Int, Int) = State[Int, Int](s => (s, s)).run(3)
    val r1 = b._1
    val s1 = b._2

    println(s1)
    println(r1)



    println("------")
    println(State.map(get[Int])(_ + 1).run(1))
    println(get[Int].map(_ + 1).map(_.toString).map(_ + "!").run(1))
  }
}