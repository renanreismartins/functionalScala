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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
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
    println(RNG.map(RNG.nonNegativeInt)(_ + 1)(Simple(1L)))

    println("double in terms of map")
    println(RNG._double(Simple(1L)))

    println("map2")
    println(RNG.map2(RNG.int, RNG.int)((a, b) => a + b)(Simple(1L)))
  }

}