package fpinscala.testing

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

  def choose(start: Int, stopExclusive: Int): Gen[Int] = ???

  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {

  type Gen[A] = State[RNG, A]

  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

