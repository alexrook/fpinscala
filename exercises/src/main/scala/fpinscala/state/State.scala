package fpinscala.state

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /** EXERCISE 6.1
    *
    * Write a function that uses RNG.nextInt to generate a random integer
    * between 0 and Int.maxValue (inclusive). Make sure to handle the corner
    * case when nextInt returns Int.MinValue, which doesn’t have a non-negative
    * counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (halfRet, newRNG) = rng.nextInt
    halfRet match {
      case Int.MinValue => math.abs(halfRet + 1) -> newRNG
      case x            => math.abs(x) -> newRNG
    }
  }

  /** EXERCISE 6.2
    *
    * Write a function to generate a Double between 0 and 1, not including 1.
    * Note: You can use Int.MaxValue to obtain the maximum positive integer
    * value, and you can use x.toDouble to convert an x: Int to a Double.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (halfRet, newRNG) = nonNegativeInt(rng)
    halfRet match {
      case m @ Int.MaxValue => (m - 1).toDouble / m -> newRNG
      case x                => (x.toDouble / Int.MaxValue) -> newRNG
    }
  }

  /** EXERCISE 6.3
    *
    * Write functions to generate an (Int, Double) pair, a (Double, Int) pair,
    * and a (Double, Double, Double) 3-tuple. You should be able to reuse the
    * functions you’ve already written.
    */

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rngN) = rng.nextInt
    val (d, rndR) = double(rngN)

    (i, d) -> rndR

  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {

    val (d, rndR) = double(rng)
    val (i, rngN) = rndR.nextInt

    (d, i) -> rndR
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rnd1) = double(rng)
    val (d2, rng2) = double(rnd1)
    val (d3, rng3) = double(rng2)

    (d1, d2, d3) -> rng3

  }

  /** EXERCISE 6.4
    *
    * Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val lb = ListBuffer.empty[Int]

    def loop(c: Int, rngN: RNG): RNG =
      if (c > 0) {
        val (i, nextRNG) = rngN.nextInt
        lb += i
        loop(c - 1, nextRNG)
      } else {
        rngN
      }

    val retRNG = loop(c = count, rngN = rng)
    lb.toList -> retRNG
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
