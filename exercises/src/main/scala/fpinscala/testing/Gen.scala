package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}
import scala.collection.mutable.ListBuffer

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

trait Prop { self =>

  /** EXERCISE 8.3
    *
    * Assuming the following representation of Prop, implement && as a method of
    * * Prop.
    */

  def check: Boolean

  def &&(p: Prop): Prop =
    new Prop {
      def check: Boolean = self.check && p.check
    }

}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A, B](f: A => B): Gen[B] = ???
  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

case class Gen_v2[A](sample: State[RNG, A])

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
