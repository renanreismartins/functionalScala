object Chap04Either {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(_) => this
      case Left(_) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
      case Right(a) => b flatMap(bb => Right(f(a, bb)))
      case Left(e) => Left(e)
    }

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      es match {
        case Nil => Right(Nil)
        case h :: t => h flatMap(hh => sequence(t) map (l => hh :: l ))
      }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as match {
        case Nil => Right(Nil)
        case h :: t => f(h) flatMap(hh => traverse(t)(f) map (l => hh :: l ))
      }

    def sequenceInTermsOfTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      traverse(es)(x => x)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  def main(args: Array[String]): Unit = {

  }

}