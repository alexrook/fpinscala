package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions
import java.{util => ju}

object Par {

  import scala.jdk.CollectionConverters._

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) =>
    UnitFuture(
      a
    ) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class WithTimeoutFuture[A](es: ExecutorService)(
      computation: => A
  ) extends Future[A] {
    var _isDone = false
    override def get(): A = computation
    override def isDone = _isDone
    def get(timeout: Long, units: TimeUnit): A = {
      val futures: ju.List[Future[A]] =
        es.invokeAll(
          List(
            new Callable[A] {
              override def call(): A = {
                val ret: A = computation
                _isDone = true
                ret
              }

            }
          ).asJava,
          timeout,
          units
        )
      futures
        .get(0) // from java util List
        .get() // method from future

    }

    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  /** EXERCISE 7.1
    *
    * Par.map2 is a new higher-order function for combining the result of two
    * parallel com- putations. What is its signature? Give the most general
    * signature possible (don’t assume it works only for Int).
    *
    * This implementation of `map2` does _not_ respect timeouts, and eagerly
    * waits for the returned futures. This means that even if you have passed in
    * "forked" arguments, using this map2 on them will make them wait. It simply
    * passes the `ExecutorService` on to both `Par` values, waits for the
    * results of the Futures `af` and `bf`, applies `f` to them, and wraps them
    * in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future`
    * implementation that records the amount of time spent evaluating `af`, then
    * subtracts that time from the available time allocated for evaluating `bf`.
    *
    * `map2` doesn't evaluate the call to `f` in a separate logical thread, in
    * accord with our design choice of having `fork` be the sole function in the
    * API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if
    * we want the evaluation of `f` to occur in a separate thread.
    */

  def map2[A, B, C](
      a: Par[A],
      b: Par[B]
  )(
      f: (A, B) => C
  ): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(
        f(af.get, bf.get)
      )
    }

  def map2_v2[A, B, C](
      ap: Par[A],
      bp: Par[B]
  )(
      f: (A, B) => C
  ): Par[C] =
    flatMap(ap) { a =>
      map(bp) { b =>
        f(a, b)
      }
    }

  /** EXERCISE 7.3 Hard
    *
    * TODO:tests
    *
    * Fix the implementation of map2 so that it respects the contract of
    * timeouts on Future.
    */
  def map2_V2[A, B, C](
      a: Par[A],
      b: Par[B]
  )(
      f: (A, B) => C
  ): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      WithTimeoutFuture(es)(f(af.get, bf.get))
    }

  /** This is the simplest and most natural implementation of `fork`, but there
    * are some problems with it for one, the outer `Callable` will block waiting
    * for the "inner" task to complete. Since this blocking occupies a thread in
    * our thread pool, or whatever resource backs the `ExecutorService`, this
    * implies that we're losing out on some potential parallelism. Essentially,
    * we're using two threads when one should suffice. This is a symptom of a
    * more serious problem with the implementation, and we will discuss this
    * later in the chapter.
    */
  def fork[A](
      a: => Par[A]
  ): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def lazyUnit[A](a: => A) = fork(unit(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get)
        t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /** EXERCISE 7.11
    *
    * Implement choiceN and then choice in terms of choiceN.
    */

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    map2(n, sequence(choices)) { case (n, list) =>
      list(n)
    }

  def choice_V2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond) { b =>
      if (b) {
        0
      } else {
        1
      }
    })(List(t, f))

  /** EXERCISE 7.12
    *
    * There’s still something rather arbitrary about choiceN. The choice of List
    * seems overly specific. Why does it matter what sort of container we have?
    * For instance, what if, instead of a list of computations, we have a Map of
    * them:
    * {{{
    *   def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V]
    * }}}
    */

  def chooser[K, V](key: Par[K])(f: K => Par[V]): Par[V] =
    es => {
      val k = key(es).get()
      f(k)(es)
    }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(choices)

  /** EXERCISE 7.13
    *
    * Implement this new primitive chooser, and then use it to implement choice
    * and choiceN.
    */

  def choiceN_V2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(key = n)(choices)

  /** EXERCISE 7.4
    *
    * TODO: tests
    *
    * This API already enables a rich set of operations. Here’s a simple
    * example: using lazyUnit, write a function to convert any function A => B
    * to one that evaluates its result asynchronously.
    */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  /** EXERCISE 7.5
    *
    * TODO: tests
    *
    * Hard: Write this function, called sequence. No additional primitives are
    * required. Do not call run.
    */
  def sequence[A](xa: List[Par[A]]): Par[List[A]] =
    xa.foldLeft(unit(List.empty[A])) { case (acc: Par[List[A]], elem: Par[A]) =>
      map2(acc, elem) { case (list: List[A], a: A) =>
        list :+ a
      }
    } // i'm using List  as accumulator instead of ListBuffer, for simplify EXERCISE.

  /** EXERCISE 7.6
    *
    * TODO: tests
    *
    * Implement parFilter, which filters elements of a list in parallel.
    */
  def parFilter[A](xa: List[A])(f: A => Boolean): Par[List[A]] =
    fork {
      map(sequence(xa.map { elem: A =>
        asyncF { // выполняем фильтр значения в отдельном потоке
          f.andThen(_ -> elem)
        }(elem)
      }))(_.collect {
        case (true, value) => // значения для котрых фильтр успешен
          value
      })
    }

  def parFilter_V2[A](xa: List[A])(f: A => Boolean): Par[List[A]] = {
    val filterF: A => Par[Boolean] = asyncF(f)
    xa.foldRight(unit(List.empty[A])) { case (elem: A, acc: Par[List[A]]) =>
      map2(filterF(elem), acc) {
        case (true, acc)  => acc :+ elem
        case (false, acc) => acc
      }
    }
  }

  /** EXERCISE 7.14
    *
    * Implement join. Can you see how to implement flatMap using join? And can
    * you implement join using flatMap?
    */

  def join[A](a: Par[Par[A]]): Par[A] =
    es => a(es).get()(es)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join {
      map(a) { a =>
        f(a)
      }
    }

  def flatMap_V2[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val ar = a(es).get()
      f(ar)(es)
    }

  def join_V2[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)

  /** EXERCISE 7.7
    *
    * Hard: Given map(y)(id) == y, it’s a free theorem that map(map(y)(g))(f) ==
    * map(y)(f compose g). (This is sometimes called map fusion, and it can be
    * used as an optimization—rather than spawning a separate parallel
    * computation to compute the second mapping, we can fold it into the first
    * mapping.)13 Can you prove it? You may want to read the paper “Theorems for
    * Free!” (http://mng.bz/Z9f1) to better under- stand the “trick” of free
    * theorems.
    */
  // map(map(y)(g))(f) == map(y)(f compose g)
  // -- if f == id
  // map(map(y)(g))(id) == map(y)(id compose g)
  // -- y = map(y)(g), so
  // map(y)(g) == map(y)(id compose g)
  // -- id compose g == g, so
  // map(y)(g) == map(y)(g)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {}
}

object Examples {
  import Par._
  def sum(
      ints: IndexedSeq[Int]
  ): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(
        ints.length / 2
      ) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(
        r
      ) // Recursively sum both halves and add the results together.
    }

}
