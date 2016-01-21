package fpinscala.datastructures

// Is this a Functor, but not a Monoid?
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
  }

  def size[A](tree: Tree[A]): Int = {
    fold[A, Int](tree)(_ => 1)((c, acc) => c + acc + 1)
  }

  def max(tree: Tree[Int]): Int = {
    fold[Int, Int](tree)(identity)(_ max _)
  }

  def depth[A](tree: Tree[A]): Int = {
    fold[A, Int](tree)(a => 0) { (c, acc) => 1 + (c max acc) }
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

}

object TreeDemo extends App {

  val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Branch(Leaf(5), Leaf(6)))))

  println("Size of " + t)
  println(Tree.size(t))

  println("Depth of " + t)
  println(Tree.depth(t))

}
