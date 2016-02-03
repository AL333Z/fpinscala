package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.laziness.Stream._
import fpinscala.state.{RNG, State}
import fpinscala.testing.Prop._

import scala.util.Left

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  self =>

  def check: Either[FailedCase, SuccessCount]

  def &&(p: Prop): Prop = new Prop {
    override def check: Either[FailedCase, SuccessCount] = (self.check, p.check) match {
      case (Right(s1), Right(s2)) => Right(s1 + s2)
      case (Right(_), Left(e)) => Left(e)
      case (Left(e), Right(_)) => Left(e)
      case (Left(e1), Left(e2)) => Left(e1 + "\n" + e2)
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int


  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}


object Gen {

  def unit[A](a: => A): Gen[A] =
    Gen(
      State.unit(a),
      Stream(a)
    )

  def booleanOld: Gen[Boolean] =
    Gen(
      State(rng => rng.nextInt match {
        case (i, rng2) => (i % 2 == 0, rng2)
      }),
      Stream(true, false)
    )

  def boolean: Gen[Boolean] =
    Gen(
      State.boolean,
      Stream(true, false)
    )

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State(RNG.nonNegativeInt).map(x => start + (x % (stopExclusive - start))),
      Stream.from(start).take(stopExclusive - start)
    )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(
      State.sequence(List.fill(n)(g.sample)),
      ???
    )

}

case class Gen[+A](sample: State[RNG, A], exhaustive: Stream[A]) {

  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

