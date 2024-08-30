package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

/** EXERCISE 12.2 Hard:
  *
  * The name applicative comes from the fact that we can formulate the
  * Applicative interface using an alternate set of primitives, unit and the
  * function apply, rather than unit and map2. Show that this formulation is
  * equivalent in expressiveness by defining map2 and map in terms of unit and
  * apply. Also establish that apply can be imple- mented in terms of map2 and
  * unit.
  */
trait Applicative_122[F[_]] extends Functor[F] { self =>

  def unit[A](a: => A): F[A]

  // have to implement this according to the task, so the functions are interdependent
  // Define in terms of map2 and unit.
  def apply_via_map2[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa) { case (f, a) =>
      f(a)
    }

  // Define in terms of apply and unit.
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply_via_map2(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val curried: A => (B => C) = f.curried
    val bToC: F[B => C] = apply_via_map2(unit(curried))(fa)
    apply_via_map2(bToC)(fb)
  }

  /** EXERCISE 12.3
    *
    * The apply method is useful for implementing map3, map4, and so on, and the
    * pattern is straightforward. Implement map3 and map4 using only unit,
    * apply, and the curried method available on functions
    */
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
      f: (A, B, C) => D
  ): F[D] = {
    val curried: A => (B => (C => D)) = f.curried
    val bToCtoD: F[B => (C => D)] = apply_via_map2(unit(curried))(fa)
    val cToD: F[C => D] = apply_via_map2(bToCtoD)(fb)
    apply_via_map2(cToD)(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] = {
    val curried: A => (B => (C => (D => E))) = f.curried
    val bToCtoDtoE: F[B => (C => (D => E))] = apply_via_map2(unit(curried))(fa)
    val cToDtoE: F[C => (D => E)] = apply_via_map2(bToCtoDtoE)(fb)
    val dToE: F[D => E] = apply_via_map2(cToDtoE)(fc)
    apply_via_map2(dToE)(fd)
  }

}
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
        list :+ b
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
  @annotation.nowarn
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) {
      case (acc: F[Map[K, V]], (k: K, v: F[V])) =>
        map2(acc, v) { case (map: Map[K, V], value: V) =>
          map.updated(k, value)
        }
    }

}

case class Tree[+A](head: A, tail: List[Tree[A]])

// Making Monad a subtype of Applicative
trait Monad[F[_]] extends Applicative[F] {
  // A minimal implementation of Monad must implement unit and override either flatMap or join and map.
  // minimal sets of operations that defined a Monad:
  // - unit and flatMap
  // - unit and compose
  // - unit, map, and join
  // here is example of flatMap via join
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  // here is example of join flatMap
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  // map via flatMap
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

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

    /** EXERCISE 12.4 Hard:
      *
      * What is the meaning of streamApplicative.sequence? Specializing the
      * signature of sequence to Stream, we have this:
      *
      * //TODO: more explanation
      */
    def sequence_explanation[A](a: List[Stream[A]]): Stream[List[A]] = {

      /** при попадании в map2 будет
        * {{{
        *   map2(acc:Stream[List[A]], //бесконечный стрим пустых листов
        *  next stream element стрим элементов
        * ) { case (list,b)=> b +: list }
        * }}}
        * каждому элементу первого стрима, будет предоставлен пустой список.
        * последующим стримам будут предоставлены списки уже заполненные
        * соответсвующими элементами предидущих стримов
        * {{{
        *  Stream(el_0_1,el_0_2,el_0_3)
        *  Stream(el_1_1,el_1_2,el_1_3)
        *  Stream(el_2_1,el_2_2,el_2_3,el_2_4) ==
        * Stream (
        *  List(el_0_1,el_1_1,el_2_1) -- первая колонка
        *  List(el_0_2,el_1_2,el_2_2) -- вторая колонка
        *  List(el_0_3,el_1_3,el_2_3) -- третья колонка
        *  el_2_4 игнорируется посколку стрим списков уже создан)
        * }}}
        *
        * List(el_2_1, el_1_1, el_0_1) List(el_2_2, el_1_2, el_0_2) List(el_2_3,
        * el_1_3, el_0_3)
        */
      ???
    }
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

object applicative_examples extends App {
  import Applicative.streamApplicative

  val r1: Stream[List[String]] = streamApplicative.sequence(
    List(
      Stream("A", "B", "C"),
      Stream("A1", "B1", "C1"),
      Stream("A2", "B2", "C2")
    )
  )

  r1.foreach(println)
  println("--------")

  val r2: Stream[List[String]] =
    streamApplicative.sequence(
      List(
        Stream("el_0_1", "el_0_2", "el_0_3"),
        Stream("el_1_1", "el_1_2", "el_1_3"),
        Stream("el_2_1", "el_2_2", "el_2_3", "el_2_4")
      )
    )

  r2.foreach(println)
  println("--------")

  val r3: Stream[List[String]] =
    streamApplicative.sequence(
      List(
        Stream("el_0_1", "el_0_2", "el_0_3", "el_0_4"),
        Stream("el_1_1", "el_1_2", "el_1_3"),
        Stream("el_2_1", "el_2_2", "el_2_3")
      )
    )

  r3.foreach(println)
  println("--------")
  val r4: Stream[List[String]] =
    streamApplicative.sequence(
      List(
        Stream("el_0_1", "el_0_2", "el_0_3", "el_0_4"),
        Stream("el_1_1", "el_1_2"),
        Stream("el_2_1", "el_2_2", "el_2_3")
      )
    )

  r4.foreach(println)

}
