package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop_v1._
import java.util.concurrent.{Executors, ExecutorService}
import scala.collection.mutable.ListBuffer
import fpinscala.state.RNG.between

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

trait Prop_v1 { self =>

  /** EXERCISE 8.3
    *
    * Assuming the following representation of Prop, implement && as a method of
    * * Prop.
    */

  def check: Boolean

  def &&(p: Prop_v1): Prop_v1 =
    new Prop_v1 {
      def check: Boolean = self.check && p.check
    }

}

object Prop_v1 {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop_v1 = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A, B](f: A => B): Gen[B] = ???
  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

case class Gen_v2[A](sample: State[RNG, A]) {

  /** EXERCISE 8.6
    *
    * Implement flatMap, and then use it to implement this more dynamic version
    * of listOfN. Put flatMap and listOfN in the Gen class.
    */
  def flatMap[B](f: A => Gen_v2[B]): Gen_v2[B] =
    Gen_v2[B] {
      sample.flatMap { a =>
        f(a).sample
      }
    }

  def listOfN(size: Gen_v2[Int]): Gen_v2[List[A]] =
    size.flatMap { int: Int =>
      Gen_v2 {
        State { rng =>
          RNG.sequence(List.fill(int)(sample.run))(rng)
        }
      }
    }
}

object Gen_v2 {

  /** Implement Gen.choose using this representation of Gen. It should generate
    * integers in the range start to stopExclusive. Feel free to use functions
    * you’ve already written.
    */
  def choose(start: Int, stopExclusive: Int): Gen_v2[Int] =
    Gen_v2 {
      State { rng: RNG =>
        rng.between(start, stopExclusive)
      }
    }

  /** EXERCISE 8.5 Let’s see what else we can implement using this
    * representation of Gen. Try implement- ing unit, boolean, and listOfN.
    */
  // Always generates the value a
  def unit[A](a: => A): Gen_v2[A] =
    Gen_v2(
      State.unit(a)
    )

  def boolean: Gen_v2[Boolean] =
    Gen_v2 {
      State { rng: RNG =>
        val (r, nextRNG) = rng.nextInt
        (r > 0) -> nextRNG
      }
    }
  // Generates lists of length n using the generator g
  def listOfN[A](n: Int, g: Gen_v2[A]): Gen_v2[List[A]] = {
    val acc = ListBuffer.empty[A]
    def loop(rng: RNG): RNG =
      if (acc.size >= n) {
        rng
      } else {
        val (a, nextRNG) = g.sample.run(rng)
        acc += a
        loop(nextRNG)
      }

    Gen_v2 {
      State { rng: RNG =>
        val next = loop(rng)
        acc.toList -> next
      }
    }
  }

  def listOfN_v2[A](n: Int, g: Gen_v2[A]): Gen_v2[List[A]] =
    Gen_v2(
      State.sequence(List.fill(n)(g.sample))
    )

  /** EXERCISE 8.7
    *
    * Implement union, for combining two generators of the same type into one,
    * by pulling values from each generator with equal likelihood.
    */
  def union[A](g1: Gen_v2[A], g2: Gen_v2[A]): Gen_v2[A] =
    boolean.flatMap { bool: Boolean =>
      if (bool) {
        g1
      } else {
        g2
      }
    }

  /** EXERCISE 8.8
    *
    * Implement weighted, a version of union that accepts a weight for each Gen
    * and generates values from each Gen with probability proportional to its
    * weight.
    */
  def weighted[A]( //TODO:test
      wg1: (Gen_v2[A], Double),
      wg2: (Gen_v2[A], Double)
  ): Gen_v2[A] = {
    val (g1: Gen_v2[A], w1: Double) = wg1
    val (g2: Gen_v2[A], w2: Double) = wg2

    val wSum: Double = w2 + w1
    val p1 = w1 / (wSum / 100)
    val p2 = w2 / (wSum / 100)

    Gen_v2 { // dice
      State {
        between(0, 100)
      }
    }.flatMap { int => // what fell out on the dice
      if (int > p1) {
        g2
      } else {
        g1
      }
    }

  }
}

trait SGen[+A] {}
 
/** EXERCISE 8.1
  *
  * To get used to thinking about testing in this way, come up with properties
  * that specify the implementation of a sum: List[Int] => Int function. You
  * don’t have to write your properties down as executable ScalaCheck code—an
  * informal description is fine. Here are some ideas to get you started: \-
  * Reversing a list and summing it should give the same result as summing the
  * original, nonreversed list.
  *   - What should the sum be if all elements of the list are the same value?
  *   - Can you think of other properties
  */

//ns.reverse.sum == ns.sum
//List.fill(10)(3).sum == 10*3
//List.empty[Number].sum == 0
//List(n, n+1, n+2,.. k) == lenght * (n + k) / 2

/** EXERCISE 8.2
  *
  * What properties specify a function that finds the maximum of a List[Int]?
  */
//ns.reverse.max == ns.max
//List.fill(a)(b).max == b
//List(a).max == a
//List.empty[Number].max == error
//List(n, n+1, n+2,.. k).max == k
