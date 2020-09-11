object Chap03Trees {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    def maximum(tree: Tree[Int]): Int = {
      def loop(t: Tree[Int], acc: Int): Int = {
        t match {
          case Leaf(v) => v max acc
          case Branch(l, r) => loop(l, acc) max loop(r, acc)
        }
      }

      loop(tree, 0)
    }

    def depth[A](t: Tree[A]): Int = {
      t match {
        case Leaf(_) => 1
        case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
      }
    }

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
      t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }
    }

//    def fold[A,B](t: Tree[A], z: B)(f: (A, B) => B): B = {
//      t match {
//        case Leaf(v) => f(v, z)
//        case Branch(l, r) => fold(l, fold(r, z)(f))(f)
//      }
//    }
//
//    def sizeInTermsOfFold[A](t: Tree[A]): Int = fold(t, 1)((a, acc) => 1 + acc )

    def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
      case Leaf(a) => f(a)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def sizeInTermsOfFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => 1 + l + r)

    //def maximum(t: Tree[Int]): Int = fold(t)(_)(_ max _) My first impl where I assumed _ could be identity.
    def maximumInTermsOfFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

    def depthInTermsOfFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => (l + 1) max (r + 1))

    def mapInTermsOfFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))
  }

  def main(args: Array[String]): Unit = {
    println("size:")
    println(Tree.size(Leaf(1)))
    println(Tree.size(Branch(Leaf(1), Leaf(2))))
    println(Tree.size(Branch(Branch(Branch(Leaf(3), Leaf(32)), Leaf(1)), Leaf(8))))

    println("size in terms of fold")
    println(Tree.sizeInTermsOfFold(Leaf(1)))
    println(Tree.sizeInTermsOfFold(Branch(Leaf(1), Leaf(2))))
    println(Tree.sizeInTermsOfFold(Branch(Branch(Branch(Leaf(3), Leaf(32)), Leaf(1)), Leaf(8))))

    println("maximum")
    println(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(5))))

    println("maximum in terms of fold")
    println(Tree.maximumInTermsOfFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(5))))

    println("depth")
    println(Tree.depth(Leaf(1)))
    println(Tree.depth(Branch(Leaf(1), Leaf(2))))
    println(Tree.depth(Branch(Branch(Branch(Leaf(9), Leaf(7)), Leaf(2)), Leaf(3))))

    println("depth in terms of fold")
    println(Tree.depthInTermsOfFold(Leaf(1)))
    println(Tree.depthInTermsOfFold(Branch(Leaf(1), Leaf(2))))
    println(Tree.depthInTermsOfFold(Branch(Branch(Branch(Leaf(9), Leaf(7)), Leaf(2)), Leaf(3))))

    println("map")
    println(Tree.map(Branch(Branch(Branch(Leaf(3), Leaf(32)), Leaf(1)), Leaf(8)))(_ + 1))

    println("map in terms of fold")
    println(Tree.mapInTermsOfFold(Branch(Branch(Branch(Leaf(3), Leaf(32)), Leaf(1)), Leaf(8)))(_ + 1))


  }

}