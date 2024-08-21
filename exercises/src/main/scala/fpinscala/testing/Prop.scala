package fpinscala.testing

import fpinscala.state.RNG
import fpinscala.state.State
import scala.collection.mutable.ListBuffer
import fpinscala.datastructures.Tree.size
import fpinscala.testing.Prop_v1.forAll
import fpinscala.datastructures.List.length
import fpinscala.errorhandling.Either.right

package object prop {

  type TestCases = Int

  type FailedCase = String

  type SuccessCount = Int

  case class Gen[A](sample: State[RNG, A]) {

    def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
      flatMap { a =>
        g.map { b =>
          f(a, b)
        }
      }

    def **[B](g: Gen[B]): Gen[(A, B)] =
      flatMap { a =>
        g.map { b =>
          a -> b
        }
      }

    // object ** {
    //   def unapply[A, B](p: (A, B)) = Some(p)
    // }

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

    def listOfMaxN[A](size: Gen[Int], g: Gen[A]): Gen[List[A]] =
      size.flatMap { s: Int =>
        Gen {
          State.sequence(List.fill(s)(g.sample))
        }
      }

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

    /** EXERCISE 8.12
      *
      * Implement a listOf combinator that doesn’t accept an explicit size. It
      * should return an SGen instead of a Gen. The implementation should
      * generate lists of the requested size.
      */

    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen { size =>
        Gen.listOfN(size, g)
      }

    /** EXERCISE 8.13
      *
      * Define listOf1 for generating nonempty lists, and then update your
      * specification of max to use this generator.
      */
    def listOf1[A](g: Gen[A]): SGen[List[A]] =
      listOf(g).flatMap { list =>
        if (list.nonEmpty) {
          SGen.unit(list)
        } else {
          SGen { _ =>
            g.map(a => List(a))
          }
        }
      }

    def listOf1_bookV[A](g: Gen[A]): SGen[List[A]] =
      SGen { size =>
        Gen.listOfN(size max 1, g)
      }

  }

  // Prop
  sealed trait Result {
    def isFalsified: Boolean

  }

  case object Proved extends Result {
    def isFalsified = false
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
          case falsified: Falsified => falsified
          case other                => that.run(testCases, rng)
        }
      }

    def ||(that: Prop): Prop =
      Prop { (testCases, rng) =>
        this.run(testCases, rng) match {
          case falsified: Falsified => that.run(testCases, rng)
          case other                => other
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

    def run(
        p: Prop,
        maxSize: Int = 100,
        testCases: Int = 100,
        rng: RNG = RNG.Simple(System.currentTimeMillis)
    ): Unit =
      p.run(testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, proved property.")
      }

  }

  /** EXERCISE 8.15 Hard: //TODO
    *
    * A check property is easy to prove conclusively because the test just
    * involves eval- uating the Boolean argument. But some forAll properties can
    * be proved as well. For instance, if the domain of the property is Boolean,
    * then there are really only two cases to test. If a property forAll(p)
    * passes for both p(true) and p(false), then it is proved. Some domains
    * (like Boolean and Byte) are so small that they can be exhaus- tively
    * checked. And with sized generators, even infinite domains can be
    * exhaustively Licensed to Alex Rook <alex.f.grach@gmail.com>140 CHAPTER 8
    * Property-based testing checked up to the maximum size. Automated testing
    * is very useful, but it’s even better if we can automatically prove our
    * code correct. Modify our library to incorporate this kind of exhaustive
    * checking of finite domains and sized generators. This is less of an exer-
    * cise and more of an extensive, open-ended design project.
    */

  /** EXERCISE 8.14
    *
    * Write a property to verify the behavior of List.sorted (API docs link:
    * http://mng.bz/ Pz86), which you can use to sort (among other things) a
    * List[Int].7 For instance, List(2,1,3).sorted is equal to List(1,2,3).
    */
  val sortedProperty: Prop =
    Prop.forAll(
      Gen.listOfMaxN(
        size = Gen.choose(0, 100),
        g = Gen.choose(Int.MinValue, Int.MaxValue)
      )
    ) { list: List[Int] =>
      val actual = list.sorted
      if (actual.isEmpty) {
        list.isEmpty
      } else {
        val smallest = list.min
        val bigest = list.max
        smallest == actual.head
        bigest == actual.last
        actual
          .foldLeft(true -> actual.head) { case ((ret, prev), next) =>
            (next >= prev) -> next
          }
          ._1
      }

    }

}
