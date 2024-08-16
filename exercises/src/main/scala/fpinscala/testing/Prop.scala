package fpinscala.testing

import fpinscala.state.RNG
import fpinscala.state.State
import scala.collection.mutable.ListBuffer
import fpinscala.datastructures.Tree.size

package object prop {

  type TestCases = Int

  type FailedCase = String

  type SuccessCount = Int

  case class Gen[A](sample: State[RNG, A]) {

    def map[B](f: A => B): Gen[B] =
      Gen {
        State { state =>
          val (a, nextRNG) = sample.run(state)
          f(a) -> nextRNG
        }
      }

    /** EXERCISE 8.6
      *
      * Implement flatMap, and then use it to implement this more dynamic
      * version of listOfN. Put flatMap and listOfN in the Gen class.
      */
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen[B] {
        sample.flatMap { a =>
          f(a).sample
        }
      }

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap { int: Int =>
        Gen {
          State { rng =>
            RNG.sequence(List.fill(int)(sample.run))(rng)
          }
        }
      }

    /** EXERCISE 8.10
      *
      * Implement helper functions for converting Gen to SGen. You can add this
      * as a method on Gen.
      */

    def unsized = SGen(_ => this)
  }

  object Gen {

    /** Implement Gen.choose using this representation of Gen. It should
      * generate integers in the range start to stopExclusive. Feel free to use
      * functions you’ve already written.
      */
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen {
        State { rng: RNG =>
          rng.between(start, stopExclusive)
        }
      }

    /** EXERCISE 8.5 Let’s see what else we can implement using this
      * representation of Gen. Try implement- ing unit, boolean, and listOfN.
      */
    // Always generates the value a
    def unit[A](a: => A): Gen[A] =
      Gen(
        State.unit(a)
      )

    def boolean: Gen[Boolean] =
      Gen {
        State { rng: RNG =>
          val (r, nextRNG) = rng.nextInt
          (r > 0) -> nextRNG
        }
      }
    // Generates lists of length n using the generator g
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val acc = ListBuffer.empty[A]
      def loop(rng: RNG): RNG =
        if (acc.size >= n) {
          rng
        } else {
          val (a, nextRNG) = g.sample.run(rng)
          acc += a
          loop(nextRNG)
        }

      Gen {
        State { rng: RNG =>
          val next = loop(rng)
          acc.toList -> next
        }
      }
    }

    def listOfN_v2[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(
        State.sequence(List.fill(n)(g.sample))
      )

    /** EXERCISE 8.7
      *
      * Implement union, for combining two generators of the same type into one,
      * by pulling values from each generator with equal likelihood.
      */
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap { bool: Boolean =>
        if (bool) {
          g1
        } else {
          g2
        }
      }

    /** EXERCISE 8.8
      *
      * Implement weighted, a version of union that accepts a weight for each
      * Gen and generates values from each Gen with probability proportional to
      * its weight.
      */
    def weighted[A]( // TODO:test
        wg1: (Gen[A], Double),
        wg2: (Gen[A], Double)
    ): Gen[A] = {
      val (g1: Gen[A], w1: Double) = wg1
      val (g2: Gen[A], w2: Double) = wg2

      val wSum: Double = w2 + w1
      val p1 = w1 / (wSum / 100)
      val p2 = w2 / (wSum / 100)

      Gen { // dice
        State {
          RNG.between(0, 100)
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

  case class SGen[A](forSize: Int => Gen[A]) {

    /** EXERCISE 8.11
      *
      * Not surprisingly, SGen at a minimum supports many of the same operations
      * as Gen, and the implementations are rather mechanical. Define some
      * convenience functions on SGen that simply delegate to the corresponding
      * functions on Gen.
      */

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      SGen[B] { size =>
        forSize(size).flatMap { a =>
          f(a).forSize(size)
        }
      }

    def map[B](f: A => B): SGen[B] =
      flatMap { a =>
        SGen.unit(f(a))
      }

    def map_v2[B](f: A => B): SGen[B] =
      SGen(size => forSize(size).map(f))
      
  }

  object SGen {
    def unit[A](a: => A): SGen[A] =
      SGen { _ =>
        Gen.unit(a)
      }
  }

  // Prop
  sealed trait Result {
    def isFalsified: Boolean

  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result {
    def isFalsified = true
  }

  case class Prop(run: (TestCases, RNG) => Result) {

    /** EXERCISE 8.9
      *
      * Now that we have a representation of Prop, implement && and || for
      * composing Prop values. Notice that in the case of failure we don’t know
      * which property was responsi ble, the left or the right. Can you devise a
      * way of handling this, perhaps by allowing Prop values to be assigned a
      * tag or label which gets displayed in the event of a failure?
      */

    def &&(that: Prop): Prop =
      Prop { (testCases, rng) =>
        this.run(testCases, rng) match {
          case Passed               => that.run(testCases, rng)
          case falsified: Falsified => falsified
        }
      }

    def ||(that: Prop): Prop =
      Prop { (testCases, rng) =>
        this.run(testCases, rng) match {
          case falsified: Falsified => that.run(testCases, rng)
          case _: Passed.type       => Passed
        }
      }
  }

  object Prop {

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
      randomStream(as)(rng)
        .zip(Stream.from(0))
        .take(n)
        .map { case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)

    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  }

}
