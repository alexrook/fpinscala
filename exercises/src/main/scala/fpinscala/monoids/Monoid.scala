package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds
import fpinscala.iomonad.IO3.Console.printLn
import fpinscala.monoids.Monoid.endoMonoid

trait Monoid[A] {
  def op(left: A, right: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  /** EXERCISE 10.1
    *
    * Give Monoid instances for integer addition and multiplication as well as
    * the Boolean operators.
    */
  val intAddition: Monoid[Int] =
    new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 + a2
      def zero: Int = 0
    }

  val intMultiplication: Monoid[Int] =
    new Monoid[Int] {
      def op(a1: Int, a2: Int): Int = a1 * a2
      def zero: Int = 1
    }

  val booleanOr: Monoid[Boolean] =
    new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
      def zero: Boolean = false
    }

  val booleanAnd: Monoid[Boolean] =
    new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
      def zero: Boolean = true
    }

  /** EXERCISE 10.2
    *
    * Give a Monoid instance for combining Option values
    *
    * @note
    *   simple monoid ignores right values for more suitable we should use
    *   optionMonoid[A:Monoid] ?
    */
  def optionMonoid[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
      def zero: Option[A] = None
    }

  /** EXERCISE 10.3
    *
    * A function having the same argument and return type is sometimes called an
    * endofunction. Write a monoid for endofunctions
    */
  def endoMonoid[A]: Monoid[A => A] =
    new Monoid[A => A] {
      def op(a1: A => A, a2: A => A): A => A =
        a1.compose(a2)
      // a1.andThen(a2)

      def zero: A => A = identity

    }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop_v1._

  /** EXERCISE 10.4
    *
    * Use the property-based testing framework we developed in part 2 to
    * implement a property for the monoid laws. Use your property to test the
    * monoids we’ve written.
    */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ??? // TODO

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.fold(m.zero)(m.op)

  /** EXERCISE 10.5
    *
    * Implement foldMap.
    */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { case (acc, el) =>
      m.op(acc, f(el))
    }

  /** EXERCISE 10.6 Hard:
    *
    * The foldMap function can be implemented using either foldLeft or fold-
    * Right. But you can also write foldLeft and foldRight using foldMap! Try
    * it.
    */

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val f1: A => (B => B) = f.curried
    val f2: B => B = foldMap(as, endoMonoid[B])(
      f1
    )
    f2(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val f1: A => (B => B) = a => b => f(b, a)
    val f2: B => B = foldMap(as, endoMonoid[B])(
      f1
    )
    f2(z)
  }

  /** EXERCISE 10.7
    *
    * Implement a foldMap for IndexedSeq. Your implementation should use the
    * strategy of splitting the sequence in two, recursively processing each
    * half, and then adding the answers together with the monoid.
    */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as match {
      case sq if sq.length > 2 =>
        val (left, right) = as.splitAt(sq.length / 2)
        val lR = foldMapV(left, m)(f)
        val rR = foldMapV(right, m)(f)
        m.op(left = lR, right = rR)

      case fist +: last +: Nil => m.op(f(fist), f(last))
      case fist +: Nil         => f(fist)
      case Nil                 => m.zero
    }

  /** EXERCISE 10.8 Hard:
    *
    * Also implement a parallel version of foldMap using the library we
    * developed in chapter 7. Hint: Implement par, a combinator to promote
    * Monoid[A] to a Monoid [Par[A]],5 and then use this to implement
    * parFoldMap.
    */
  import fpinscala.parallelism.Nonblocking._

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]] {
      def op(left: Par[A], right: Par[A]): Par[A] =
        Par.map2(left, right)(m.op)

      def zero: Par[A] = Par.unit(m.zero)
    }

  // TODO: split parallel
  def parFoldMap_V1[A, B](xa: IndexedSeq[A], m: Monoid[B])(
      f: A => B
  ): Par[B] = {
    val parM: Monoid[Par[B]] = par(m)
    foldMapV(xa, parM)(a => Par.unit(f(a)))
  }

  def parFoldMap_V2[A, B](xa: IndexedSeq[A], m: Monoid[B])(
      f: A => B
  ): Par[B] = {
    val parM: Monoid[Par[B]] = par(m)

    def loop(xa: IndexedSeq[A]): Par[B] =
      xa match {
        case Nil => Par.unit(m.zero)

        case first +: Nil => Par.delay(f(first))

        case sq =>
          val parSplitted: Par[(IndexedSeq[A], IndexedSeq[A])] =
            Par.delay(sq.splitAt(sq.length / 2))

          Par.flatMap(parSplitted) {
            case (left: IndexedSeq[A], right: IndexedSeq[A]) =>
              val lR: Par[B] = loop(left)
              val rR: Par[B] = loop(right)
              parM.op(left = lR, right = rR)
          }

      }

    loop(xa)

  }

  /** EXERCISE 10.9 Hard
    *
    * Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll
    * need to come up with a creative Monoid.
    */
  def ordered(ints: IndexedSeq[Int]): Boolean = ??? // TODO

  sealed trait WC
  case class Stub(chars: String) extends WC
  // case class Part(lStub: String, words: Int, rStub: String) extends WC
  case class Part_v2(lStub: Boolean, words: Int, rStub: Boolean) extends WC

  /** EXERCISE 10.10
    *
    * Write a monoid instance for WC and make sure that it meets the monoid
    * laws.
    */

  lazy val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero) { case (b, a) =>
      mb.op(b, f(a))
    }

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldMap(as)(identity)(m)

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(List.empty[A]) { case (acc, a) =>
      a +: acc
    }.reverse

}

/** EXERCISE 10.12
  *
  * Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
  * Remember that foldRight, foldLeft, and foldMap can all be implemented in
  * terms of each other, but that might not be the most efficient
  * implementation.
  */
object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = {
    def loop(acc: B, xa: List[A]): B =
      xa match {
        case head :: next =>
          loop(f(head, acc), next)
        case Nil => acc
      }
    loop(z, as.reverse)
  }

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = {
    def loop(acc: B, xa: List[A]): B =
      xa match {
        case head :: next =>
          loop(f(acc, head), next)
        case Nil => acc
      }
    loop(z, as)
  }
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero) { case (b, a) =>
      mb.op(b, f(a))
    }
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = {
    val f1: A => (B => B) = f.curried

    foldMap(as)(f1.apply)(endoMonoid)(z)
  }

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = {
    val f1: A => B => B = a => b => f(b, a)
    foldMap(as)(f1.apply)(endoMonoid)(z)
  }

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)

}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    ???
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    ???
}

object MonoidTest extends App { // TODO: replace with testing via Prop

  // Associative law
  def assocLaw[A](x: A, y: A, z: A)(
      monoid: Monoid[A],
      compare: (A, A) => Boolean = (a1: A, a2: A) => a1 == a2
  ): Boolean = {
    val ret =
      compare(
        monoid.op(monoid.op(x, y), z),
        monoid.op(x, monoid.op(y, z))
      )

    if (ret) {
      ret
    } else {
      println(s"The folowing args breaks assoc law[$x, $y, $z]")
      ret
    }
  }

  // id element law
  def zeroLaw[A](x: A)(
      monoid: Monoid[A],
      compare: (A, A) => Boolean = (a1: A, a2: A) => a1 == a2
  ): Boolean = {
    import monoid._
    val ret =
      compare(
        op(x, zero),
        x
      ) &&
        compare(
          op(zero, x),
          x
        )

    if (ret) {
      ret
    } else {
      println(s"The folowing arg breaks zero law[$x]")
      ret
    }
  }

  import Monoid._
  def test_option(): Unit = {
    val al = assocLaw(None, Some(1), Some(3))(optionMonoid) &&
      assocLaw(Some(2): Option[Int], Some(1), Some(3))(optionMonoid) &&
      assocLaw(Some(2), None, Some(3))(optionMonoid) &&
      assocLaw(Some(2), Some(1), None)(optionMonoid) &&
      assocLaw(Some(2), None, None)(optionMonoid) &&
      assocLaw(None, Option.empty[Int], None)(optionMonoid) &&
      assocLaw(None, None, Some(2))(optionMonoid)

    println(s"OptionMonoid Assoc Law[$al]")

    val zl = zeroLaw(None: Option[Int])(optionMonoid) && zeroLaw(
      Some("A"): Option[String]
    )(optionMonoid)
    println(s"OptionMonoid Zero Law[$zl]")

  }

  // test_option()

  def testEndoMonoid(): Unit = {
    val f1: Int => Int = _ + 1
    val f2: Int => Int = _ - 4
    val f3: Int => Int = _ - 45

    def compare(v: Int): (Int => Int, Int => Int) => Boolean =
      (f1, f2) => f1(v) == f2(v)

    val tf1: Int => Int =
      endoMonoid.op(endoMonoid.op(f1, f2), f3)

    val tf2: Int => Int =
      endoMonoid.op(f1, endoMonoid.op(f2, f3))

    val r = tf1(125)
    println(r)
    println(s"tf1(17)[${tf1(23)}], tf2(17)[${tf2(17)}]")

    val al =
      assocLaw(f1, f2, f3)(endoMonoid, compare(1)) &&
        assocLaw(f3, f1, f2)(endoMonoid, compare(2)) &&
        assocLaw(f2, f3, f1)(endoMonoid, compare(3)) &&
        assocLaw(f1, f3, f2)(endoMonoid, compare(4))

    println(s"EndoMonoid Assoc Law[$al]")

    val zl =
      zeroLaw(f1)(endoMonoid, compare(1)) &&
        zeroLaw(f2)(
          endoMonoid,
          compare(2)
        ) && zeroLaw(f3)(endoMonoid, compare(3)) &&
        zeroLaw(identity[Int] _)(
          endoMonoid,
          compare(4)
        )

    println(s"EndoMonoid Zero Law[$al]")
  }

  // testEndoMonoid()

  println(count("lorem ipsum dolor sit amet, "))

}
