package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] { self =>

  /** EXERCISE 12.1
    *
    * Transplant the implementations of as many combinators as you can from
    * Monad to Applicative, using only map2 and unit, or methods implemented in
    * terms of them.
    */

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ???

  // EXERCISE 12.1
  @annotation.nowarn
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa) { case (f: (A => B), a: A) =>
      f(a)
    }

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  // EXERCISE 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)
  // EXERCISE 12.1
  @annotation.nowarn // disabled for eduction purposes
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldLeft(unit(List.empty[B])) { case (acc: F[List[B]], elem: A) =>
      map2(acc, f(elem)) { case (list, b) =>
        b +: list
      }
    }

  // EXERCISE 12.1
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence {
      List.fill(n)(fa)
    }

  // EXERCISE 12.1
  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)(_ -> _)

  // EXERCISE 12.1
  def product[G[_]](
      g: Applicative[G]
  ): Applicative[({ type LR[x] = (F[x], G[x]) })#LR] =
    new Applicative[({ type LR[x] = (F[x], G[x]) })#LR] {
      def unit[A](a: => A): (F[A], G[A]) = self.unit(a) -> g.unit(a)
      override def map2[A, B, C](left: (F[A], G[A]), right: (F[B], G[B]))(
          f: (A, B) => C
      ): (F[C], G[C]) = {
        val (fa: F[A], ga: G[A]) = left
        val (fb: F[B], gb: G[B]) = right
        val r1: F[C] = self.map2(fa, fb)(f)
        val r2: G[C] = g.map2(ga, gb)(f)
        r1 -> r2
      }
    }

  // EXERCISE 12.1
  def compose[G[_]](
      g: Applicative[G]
  ): Applicative[({ type CO[x] = F[G[x]] })#CO] =
    new Applicative[({ type CO[x] = F[G[x]] })#CO] {
      def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(
          f: (A, B) => C
      ): F[G[C]] = {
        val (ga: G[A], gb: G[B]) = self.map2(fa, fb)(_ -> _)
        val gc: G[C] = g.map2(ga, gb)(f)
        self.unit(gc)
      }

    }
  // EXERCISE 12.1
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) {
      case (acc: F[Map[K, V]], (k: K, v: F[V])) =>
        map2(acc, v) { case (map: Map[K, V], value: V) =>
          map.updated(k, value)
        }
    }

}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] = ???

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(
        f: A => State[S, B]
    ): State[S, B] =
      st flatMap f
  }

  def composeM[F[_], N[_]](implicit
      F: Monad[F],
      N: Monad[N],
      T: Traverse[N]
  ): Monad[({ type f[x] = F[N[x]] })#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](
        a: Stream[A],
        b: Stream[B]
    )( // Combine elements pointwise
        f: (A, B) => C
    ): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]
      : Applicative[({ type f[x] = Validation[E, x] })#f] = ???

  type Const[A, B] = A

  @annotation.nowarn
  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A, B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](as)(f)(
      monoidApplicative(mb)
    )

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      (for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- StateUtil.set(s2)
      } yield b)
    ).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit
      G: Applicative[G],
      H: Applicative[H]
  ): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit
      G: Traverse[G]
  ): Traverse[({ type f[x] = F[G[x]] })#f] = ???
}

object Traverse {
  val listTraverse = ???

  val optionTraverse = ???

  val treeTraverse = ???
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
