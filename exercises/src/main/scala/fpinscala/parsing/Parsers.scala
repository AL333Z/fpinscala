package fpinscala.parsing

import fpinscala.testing.exhaustive.Prop._
import fpinscala.testing.exhaustive.{Gen, Prop}

import scala.language.{higherKinds, implicitConversions}

// so inner classes may call methods of trait
trait Parsers[Parser[+ _]] {
  self =>
  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  // this looks like a return..
  def succeed[A](a: A): Parser[A]
  // def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def slice[A](p: Parser[A]): Parser[String]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  def many[A](p: Parser[A]): Parser[List[A]] =
    p.map2(many(p))(_ :: _) or succeed(List[A]())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    p.map2(many(p))(_ :: _)

  def map[A, B](pa: Parser[A])(f: A => B): Parser[B] =
    pa.flatMap(a => succeed(f(a)))

  def map2[A, B, C](pa: Parser[A])(pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    pa.flatMap(a => pb.map(b => f(a, b)))

  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
    map2(pa)(pb)((a, b) => (a, b))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n > 0) p.map2(listOfN(n - 1, p))(_ :: _)
    else succeed(List[A]())
  }

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = p.product(p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p)(p2)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}