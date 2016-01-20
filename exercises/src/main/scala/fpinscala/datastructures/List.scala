package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case xs => xs
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case xs => xs
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => setHead(init(xs), x)
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((acc, elem) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(List.reverse(l), z) { (a, b) => f(b, a) }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(List.reverse(as), z) { (b, a) => f(a, b) }

  def reverse[A](l: List[A]): List[A] = {
    foldRight(l, Nil: List[A]) { (x, acc) => append(acc, Cons(x, Nil)) }
  }

  def reverseViaFoldLeft[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A]) { (acc, x) => append(Cons(x, Nil), acc) }
  }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldLeft(l, Nil: List[B]) { (acc, x) => append(acc, Cons(f(x), Nil)) }

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) Cons(x, Nil) else Nil)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    val x: List[List[B]] = map(l)(f)
    foldRight(x, Nil: List[B])(append)
  }

  // expanding foldRight with a f
  val list = Cons(1, Cons(2, Cons(3, Nil)))
  val listSum = sum2(list)

  // expands to:
  foldRight(list, 0)(_ + _)
  val step1 = 1 + foldRight(list.tail, 0)(_ + _)
  // list.tail == (2, 3, Nil)
  val step2 = 1 + (2 + foldRight(list.tail, 0)(_ + _))
  // list.tail == (3, Nil)
  val step3 = 1 + (2 + (3 + foldRight(list.tail, 0)(_ + _)))
  // list.tail == (Nil)
  val step4 = 1 + (2 + (3 + 0))
  // ...
}

object Demo extends App {
  val list: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

  println("Reverse via foldRight of " + list)
  println(List.reverse(list))

  println("Reverse via foldLeft of " + list)
  println(List.reverseViaFoldLeft(list))

  println("Concat with foldLeft")
  println(List.foldLeft(list, Nil: List[Int])((acc, x) => List.append[Int](acc, Cons(x, Nil))))

  println("Concat with foldLeft via foldRight")
  println(List.foldLeftViaFoldRight(list, Nil: List[Int])((acc, x) => List.append[Int](acc, Cons(x, Nil))))

  println("Concat with foldRight")
  println(List.foldRight(list, Nil: List[Int])((x, acc) => List.append[Int](Cons(x, Nil), acc)))

  println("Concat with foldRight via foldLeft")
  println(List.foldRightViaFoldLeft(list, Nil: List[Int])((x, acc) => List.append[Int](Cons(x, Nil), acc)))

  println("Map with foldLeft")
  println(List.map(list)(_ + 1))

  println("FlatMap")
  println(List.flatMap(List(1, 2, 3))(i => List(i, i)))

}