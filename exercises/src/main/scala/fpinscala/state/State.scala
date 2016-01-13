package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = // pass the rng state without using it
    rng => (a, rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    //  def flatMap[A, B](f: RNG => (A, RNG))(g: A => (RNG => (B, RNG))): RNG => (B, RNG) = {
    rng => {
      val (a, rnga): (A, RNG) = f(rng)
      g(a)(rnga)
    }
  }

  def mapOld[A, B](s: Rand[A])(f: A => B): Rand[B] =
  //  def map[A,B](s: RNG => (A, RNG))(f: A => B): RNG => (B, RNG) =
    rng => {
      val (a, rng2) = s(rng) // (A, RNG)
      (f(a), rng2) // (B, RNG)
    }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a => unit(f(a)) }

  def map2Old[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    //  def map2[A, B, C](ra: RNG => (A, RNG), rb: RNG => (B, RNG))(f: (A, B) => C): RNG => (C, RNG) = {
    rng => {
      val (a, rnda) = ra(rng)
      val (b, rndb) = rb(rnda)
      val c = f(a, b)
      (c, rndb)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b => unit(f(a, b)) }
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    //    def sequence[A](fs: List[RNG => (A, RNG)]): RNG => (List[A], RNG) = {
    fs.foldRight(unit(List[A]())) { (ra: Rand[A], acc: Rand[List[A]]) =>
      map2(ra, acc)(_ :: _)
    }
  }

  def nonNegativeIntOld(rng: RNG): (Int, RNG) = {
    val (n, nextS) = rng.nextInt
    if (n == Integer.MIN_VALUE) nonNegativeInt(nextS) // MIN_VALUE has not a positive counterpart
    else (n.abs, nextS)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    flatMap(int) { n =>
      if (n == Integer.MIN_VALUE) nonNegativeInt // MIN_VALUE has not a positive counterpart
      else unit(n)
    }(rng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val rand: Rand[Double] = map(nonNegativeInt) { n => n / Integer.MAX_VALUE.toDouble + 1 }
    rand(rng)
  }

  def doubleOld(rng: RNG): (Double, RNG) = {
    val (n, nextS) = nonNegativeInt(rng)
    (n / Integer.MAX_VALUE.toDouble + 1, nextS)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nInt, nextS) = nonNegativeInt(rng)
    val (nDouble, nextS2) = double(nextS)
    ((nInt, nDouble), nextS2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (nDouble, nextS) = double(rng)
    val (nInt, nextS2) = nonNegativeInt(nextS)
    ((nDouble, nInt), nextS2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (nDouble1, nextS) = double(rng)
    val (nDouble2, nextS2) = double(nextS)
    val (nDouble3, nextS3) = double(nextS2)
    ((nDouble1, nDouble2, nDouble3), nextS3)
  }

  def intsOld(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((List[Int](), rng)) { (currentState, i) =>
      val (l, currentGen) = currentState
      val (next, nextGen) = currentGen.nextInt
      (next +: l, nextGen)
    }
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def positiveMax(n: Int): Rand[Int] =
    map(nonNegativeInt) { x => x % (n + 1) }

}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap { a => unit(f(a)) }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.flatMap { b =>
        unit(f(a, b))
      }
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, sa) = run(s)
      val stateb: State[S, B] = f(a)
      stateb.run(sa)
    }

  def unit[B](b: B): State[S, B] = State(s => (b, s))

  def sequence[B](fs: List[State[S, B]]): State[S, List[B]] =
    fs.foldRight(unit(List[B]())) { (sa: State[S, B], acc: State[S, List[B]]) =>
      sa.map2(acc)(_ :: _)
    }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
