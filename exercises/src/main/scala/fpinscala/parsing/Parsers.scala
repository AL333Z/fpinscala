package fpinscala.parsing

import fpinscala.testing.exhaustive.Prop._
import fpinscala.testing.exhaustive.{SGen, Gen, Prop}

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

// so inner classes may call methods of trait
trait Parsers[Parser[+ _]] {
  self =>
  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def andThen[A, B](a: Parser[A], b: Parser[B]): Parser[B] =
    a.flatMap(_ => b)

  // this looks like a return..
  def succeed[A](a: A): Parser[A]

  // def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def slice[A](p: Parser[A]): Parser[String]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  // if p fails, its ParseError will somehow incorporate msg
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def errorStack(e: ParseError): List[(Location, String)]

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  def attempt[A](p: Parser[A]): Parser[A]

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

  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = p.product(p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p)(p2)(f)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def slice: Parser[String] = self.slice(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def andThen[B](b: Parser[B]): Parser[B] = self.andThen(p, b)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      }
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

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

}

object Demo {

  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val spaces = char(' ').many.slice
    val digit = "[0-9]".r
    val digitFrom1 = "[1-9]".r
    val letter = "[A-Za-z]".r
    val alphanum = (digit or letter).many1

    val openBracket = char('{')
    val closeBracket = char('}')
    val quote = char('\"')
    val openSquareBracket = char('[')
    val closeSquareBracket = char(']')
    val eq = char('=')

    val number = digitFrom1 andThen digit.many
    val literal = quote andThen alphanum andThen quote
    val boolean = string("true") or string("false")
    val jnull = string("null")

    val array = openSquareBracket andThen closeSquareBracket

    val jobject = for {
      _ <- openBracket
      _ <- literal
      _ <- eq
      _ <- number or boolean or jnull or array or jobject
      _ <- closeBracket
    } yield ()

    ???
  }
}