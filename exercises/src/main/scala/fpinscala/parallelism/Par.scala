package fpinscala.parallelism

import java.util.concurrent._

import scala.language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that
  // just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled.
  // Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having
  // `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we
  // want the evaluation of `f` to occur in a separate thread.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    map(product(a, b)) { t => f(t._1, t._2) }

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the
  // outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our
  // thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential
  // parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious
  // problem with the implementation, and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit(new Callable[A] {
      override def call(): A = run(es)(a).get // !?
    })
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = es => {
    val resa = run(es)(pa)
    UnitFuture(f(resa.get))
  }

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = es => {
    val resa = run(es)(fa)
    val resb = run(es)(fb)
    UnitFuture(resa.get, resb.get)
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a =>
    map(unit(a))(f)

  def sortPar(parList: Par[List[Int]]) =
    map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => {
      if (run(es)(cond).get) t(es) // !?
      else f(es)
    }

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {}
}

object Examples {
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
