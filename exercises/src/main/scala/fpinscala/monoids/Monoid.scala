package fpinscala.monoids

import fpinscala.monoids.Monoid.WC
import fpinscala.parallelism.Nonblocking._
import fpinscala.testing.exhaustive.{Gen, Prop}

import scala.language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {

    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    override def zero: A => A = identity
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {

    val identityLaw = Prop.forAll(gen) { a =>
      m.op(m.zero, a) == m.op(a, m.zero)
    } tag "monoid identity law"

    lazy val values: Gen[(A, A, A)] = for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)

    lazy val associativeLaw = Prop.forAll(values) { case (x, y, z) =>
      m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
    } tag "monoid associative law"

    identityLaw && associativeLaw
  }

  /*
    A monoid homomorphism f between monoids M and N obeys the
    following general law for all values x and y:
    M.op(f(x), f(y)) == f(N.op(x, y))

    e.g:
      f: (String => Int) // string length fn
      M: Monoid[Int] // int addition monoid
      N: Monoid[String]
   */

  val wordsMonoid: Monoid[String] = new Monoid[String] {

    private def cleanString(s: String) = s.dropWhile(_.isSpaceChar).takeWhile(_.isSpaceChar)

    override def op(a1: String, a2: String): String = cleanString(a1) + " " + cleanString(a2)

    override def zero: String = stringMonoid.zero

  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.fold(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B])) { a => b => f(b, a) }(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC

  /*
    A Stub is the simplest case, where we have not seen any complete words yet.
   */
  case class Stub(chars: String) extends WC

  /*
  Part keeps the number of complete words we have seen so far, in words.
  The value lStub holds any partial word we have seen to the left of those words,
  and rStub holds the ones on the right.
   */
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    sys.error("todo")

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    sys.error("todo")

  val wcMonoid: Monoid[WC] = new Monoid[WC] {

    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(x), Stub(y)) => Stub(x + y)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
      case (Part(l1, w1, r1), Stub(y)) => Part(l1, w1, r1 + y)
      case (Stub(x), Part(l2, w2, r2)) => Part(x + l2, w2, r2)
    }

    override def zero: WC = Stub("")
  }

  def count(s: String): Int = {

    def wc(char: Char): WC = {
      if (!char.isWhitespace) Stub(char.toString)
      else Part("", 0, "")
    }

    // given a string in a stub, is it a word?
    def unstub(x: String): Int = x.length.min(1)

    val resWC = foldMapV(s.toIndexedSeq, wcMonoid)(wc)
    resWC match {
      case Stub(x) => unstub(x)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as) { a => (b: B) => f(b, a) }(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldMap(as)(identity)(m)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _

}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero) { (b, a) => mb.op(f(a), b) }

  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }


//  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
//    case Leaf(a) => f(z, a)
//    case Branch(l, r) => {
//      // why is the compiler crazy here?
//      val inner: B = foldLeft(l)(z)(f)
//      foldLeft(r)(inner)(f)
//    }
//  }
//
//  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
//    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}

object Demo extends App {

  Prop.run(Monoid.monoidLaws(Monoid.intAddition, Gen.smallInt).tag("IntAdditionMonoid"))
  Prop.run(Monoid.monoidLaws(Monoid.intMultiplication, Gen.smallInt).tag("IntMultiplicationMonoid"))

  Prop.run(Monoid.monoidLaws(Monoid.booleanAnd, Gen.boolean).tag("BooleanAndMonoid"))
  Prop.run(Monoid.monoidLaws(Monoid.booleanOr, Gen.boolean).tag("BooleanOrMonoid"))

  Prop.run(Monoid.monoidLaws(Monoid.wordsMonoid, Gen.stringN(10)).tag("WordsMonoid"))

  val wcMonoid: Monoid[WC] = Monoid.wcMonoid
  Prop.run(Monoid.monoidLaws(Monoid.wcMonoid, Gen.stringN(10)).tag("WordsMonoid"))


  Prop.run(Monoid.monoidLaws(Monoid.optionMonoid[Int], Gen.smallInt.map(x => if (x % 2 == 0) Some(x) else None)).tag("OptionMonoid"))

  // ???
  //  Prop.run(Monoid.monoidLaws(Monoid.endoMonoid[Int], Gen.smallInt.map(x => (y:Int) => x)).tag("EndoMonoid"))

}
