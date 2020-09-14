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

  def main(args: Array[String]): Unit = {
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
    println(Some("Renan").orElse(Some("no value") ))
    println(None.orElse(Some("no value")))
    println()

    println("filter")
    println(Some("Renan").filter(_.length > 3))
    println(Some("Renan").filter(_.length > 10))
    println(None.filter(_ => true))
    println()
  }
}