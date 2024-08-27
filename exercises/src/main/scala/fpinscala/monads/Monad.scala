package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import fpinscala.state._
import parallelism.Par._
import language.higherKinds
import java.lang

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /** We wrote this just by following the types, but let’s think about what it
    * means for con- crete data types like List, Gen, Option, and so on. For
    * example, if we distribute a List[(A, B)], we get two lists of the same
    * length, one with all the As and the other with all the Bs. That operation
    * is sometimes called unzip
    */
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa)  => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /** EXERCISE 11.3
    *
    * The sequence and traverse combinators should be pretty familiar to you by
    * now, and your implementations of them from various prior chapters are
    * probably all very similar. Implement them once and for all on Monad[F].
    */
  @annotation.nowarn // unchecked suppressed for for educational purposes
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldLeft(unit(List.empty[A])) { case (acc: M[List[A]], m: M[A]) =>
      flatMap(acc) { list: List[A] =>
        map(m)(a => list :+ a)
      }
    }

  @annotation.nowarn
  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldLeft(unit(List.empty[B])) { case (acc: M[List[B]], a: A) =>
      flatMap(acc) { list: List[B] =>
        val mb: M[B] = f(a)
        map(mb)(b => list :+ b)
      }
    }

  /** EXERCISE 11.4
    *
    * One combinator we saw for Gen and Parser was listOfN, which allowed us to
    * repli- cate a parser or generator n times to get a parser or generator of
    * lists of that length.
    *
    * We can implement this combinator for all monads F by adding it to our
    * Monad trait. We should also give it a more generic name such as replicateM
    * (meaning “replicate in a monad”).
    *
    * Implement replicateM.
    */
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    map(ma) { a =>
      List.fill(n)(a)
    }

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = ???

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ???

  def join[A](mma: M[M[A]]): M[A] = ???

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  /** EXERCISE 11.1
    *
    * Write monad instances for Par, Parser, Option, Stream, and List.
    */

  val parMonad: Monad[Par] =
    new Monad[Par] {
      def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
      def unit[A](a: => A): Par[A] = Par.unit(a)
    }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ??? // TODO:impl

  val optionMonad: Monad[Option] =
    new Monad[Option] {
      def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
        ma.flatMap(f)

      def unit[A](a: => A): Option[A] = Option(a)

    }

  val streamMonad: Monad[laziness.Stream] =
    new Monad[laziness.Stream] {
      def flatMap[A, B](ma: laziness.Stream[A])(
          f: A => laziness.Stream[B]
      ): laziness.Stream[B] = ma.flatMap(f)
      def unit[A](a: => A): laziness.Stream[A] = laziness.Stream.apply(a)

    }

  val listMonad: Monad[List] =
    new Monad[List] {
      def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
      def unit[A](a: => A): List[A] = List(a)
    }

  /** EXERCISE 11.2 Hard:
    *
    * State looks like it would be a monad too, but it takes two type arguments
    * and you need a type constructor of one argument to implement Monad. Try to
    * implement a State monad, see what issues you run into, and think about
    * possible solutions.
    */

  def stateMonad[S] = {
    type S1[A] = State[S, A]
    new Monad[S1] {
      def flatMap[A, B](ma: S1[A])(f: A => S1[B]): S1[B] =
        ma.flatMap(f)
      def unit[A](a: => A): S1[A] = State.unit(a)
    }
  }

  class StateMonads[S] {
    type StateS[A] = State[S, A]

    // We can then declare the monad for the `StateS` type constructor:
    val monad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A, B](st: State[S, A])(
          f: A => State[S, B]
      ): State[S, B] =
        st flatMap f
    }
  }

  lazy val example_monad_family = {
    val m = new StateMonads[String].monad
    m.unit(1).flatMap(???)
  }

  // But we don't have to create a full class like `StateMonads`. We can create
  // an anonymous class inline, inside parentheses, and project out its type member,
  // `lambda`:
  def stateMonad_Book[S] =
    new Monad[({ type lambda[x] = State[S, x] })#lambda] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A, B](st: State[S, A])(
          f: A => State[S, B]
      ): State[S, B] =
        st flatMap f
    }

  val idMonad: Monad[Id] = ???

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???
  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = ???
    override def flatMap[A, B](st: Reader[R, A])(
        f: A => Reader[R, B]
    ): Reader[R, B] = ???
  }
}
