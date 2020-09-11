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

  }

  def main(args: Array[String]): Unit = {
    println(Tree.size(Leaf(1)))
    println(Tree.size(Branch(Leaf(1), Leaf(2))))
    println(Tree.size(Branch(Branch(Branch(Leaf(3), Leaf(32)), Leaf(1)), Leaf(8))))
    println(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(5))))

    println("depth")
    println(Tree.depth(Leaf(1)))
    println(Tree.depth(Branch(Leaf(1), Leaf(2))))
    println(Tree.depth(Branch(Branch(Branch(Leaf(9), Leaf(7)), Leaf(2)), Leaf(3))))

    println("map")
    println(Tree.map(Branch(Branch(Branch(Leaf(3), Leaf(32)), Leaf(1)), Leaf(8)))(_ + 1))

  }

}