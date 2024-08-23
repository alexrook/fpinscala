package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds
import fpinscala.iomonad.IO3.Console.printLn

trait Monoid[A] {
  def op(a1: A, a2: A): A
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
      //  a1.andThen(a2)

      def zero: A => A = identity

    }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop_v1._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    ???

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    ???

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    ???

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    ???

  lazy val wcMonoid: Monoid[WC] = ??? // TODO: remove lazy after impl

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

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    ???
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    ???
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

  test_option()

  def testEndoMonoid(): Unit = {
    val f1: Int => Int = _ + 1
    val f2: Int => Int = _ - 4
    val f3: Int => Int = _ * 45

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

  testEndoMonoid()

}
