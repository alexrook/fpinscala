package fpinscala
package applicative

import monads.Functor
import state._
import monoids._
import language.higherKinds
import language.implicitConversions
import scala.util.control.NonFatal
import fpinscala.datastructures.List.tail
import fpinscala.monads.Id

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

trait Applicative_Alt[F[_]] {
  // product, map, and unit are an alternate formulation of Applicative. Can you see how map2 can be imple-
  // mented using product and map?

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    map(product(fa, fb)) { case (a, b) =>
      f(a, b)
    }

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def unit[A](a: => A): F[A]

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

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

  def map_v2[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(f))((a, f) => f(a))

  def map_v3[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))
  // from above
  // {{
  //   map2(unit(()), fa)((_,a) => a) == fa
  //   map2(fa, unit(()))((a,_) => a) == fa
  // }}

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

  /** EXERCISE 12.1 + EXERCISE 12.8
    *
    * Just like we can take the product of two monoids A and B to give the
    * monoid (A, B), we can take the product of two applicative functors.
    * Implement this function:
    */
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

  /** EXERCISE 12.1 + EXERCISE 12.9 Hard:
    *
    * Applicative functors also compose another way! If F[_] and G[_] are
    * applicative functors, then so is F[G[_]]. Implement this function:
    */
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

  /** EXERCISE 12.10 Hard:
    *
    * Prove that {{{Applicative[({ type CO[x] = F[G[x]] })#CO]}}} composite
    * applicative functor meets the applicative laws. This is an extremely
    * challenging exercise. TODO: impl
    */

  /** EXERCISE 12.1 + EXERCISE 12.12
    *
    * On the Applicative trait, implement sequence over a Map rather than a
    * List:
    */
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

  /** EXERCISE 12.11
    *
    * Try to write compose on Monad. It’s not possible, but it is instructive to
    * attempt it and understand why this is the case.
    */
  def compose[G[_]](g: Monad[G]): Monad[({ type MM[x] = F[G[x]] })#MM] = {
    val self = this

    new Monad[({ type MM[x] = F[G[x]] })#MM] {
      def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))

      override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        self.flatMap(ma) { ga: G[A] =>
          val r: G[F[G[B]]] = g.map(ga) { a =>
            f(a)

          }
          // It’s not possible
          // потому, что не возможно развернуть G[_]  в F.flatMap
          // а также, не возможно развернуть F[_] в G.flatMap
          ???

        }

    }
  }

}

object Monad {

  /** EXERCISE 12.5
    *
    * Write a monad instance for Either.
    */
  def eitherMonad[E]: Monad[({ type EE[x] = Either[E, x] })#EE] =
    new Monad[({ type EE[x] = Either[E, x] })#EE] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](ma: Either[E, A])(
          f: A => Either[E, B]
      ): Either[E, B] = ma flatMap f
    }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(
        f: A => State[S, B]
    ): State[S, B] =
      st flatMap f
  }

  /** EXERCISE 12.20 Hard:
    *
    * Implement the composition of two monads where one of them is traversable.
    */

  def composeM[F[_], N[_]](implicit
      fm: Monad[F],
      nm: Monad[N],
      traversableN: Traverse[N]
  ): Monad[({ type f[x] = F[N[x]] })#f] =
    new Monad[({ type f[x] = F[N[x]] })#f] {
      def unit[A](a: => A): F[N[A]] = fm.unit(nm.unit(a))

      override def flatMap[A, B](ma: F[N[A]])(f: A => F[N[B]]): F[N[B]] =
        fm.flatMap(ma) { na: N[A] =>
          val r: F[N[N[B]]] = traversableN.traverse(na)(f)
          fm.map(r) { nnb =>
            nm.join(nnb)
          }
        }

    }
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
    val sequence_explanation = {

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
      /** Again:
        * {{{
        *  Stream(List.empty,List.empty,List.empty,List.empty,...,,,.infinity)
        *   zip //соединяет только те элементы которые имеют пару в другом стриме, остальные удаляет
        *  Stream(el_0_1,el_0_2,el_0_3)
        *   ||
        *  Stream(List(el_0_1),List(el_0_2),List(el_0_3))
        *   zip
        *  Stream(el_1_1,el_1_2,el_1_3)
        *   ||
        *  Stream(List(el_0_1,el_1_1),List(el_0_2,el_1_2),List(el_0_3,el_1_3))
        *   zip
        *   ....
        * }}}
        */
      "Done"
    }
  }

  /** EXERCISE 12.6
    *
    * Write an Applicative instance for Validation that accumulates errors in
    * Failure. Note that in the case of Failure there’s always at least one
    * error, stored in head. The rest of the errors accumulate in the tail.
    */
  def validationApplicative[E]
      : Applicative[({ type VV[x] = Validation[E, x] })#VV] =
    new Applicative[({ type VV[x] = Validation[E, x] })#VV] {
      def unit[A](a: => A): Validation[E, A] =
        try {
          Success(a)
        } catch {
          case NonFatal(e: E) =>
            Failure(head = e, tail = Vector.empty[E])
        }

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
          f: (A, B) => C
      ): Validation[E, C] =
        fa match {
          case err @ Failure(faHead, faTail) =>
            fb match {
              case Failure(fbHead, fbTail) =>
                Failure(faHead, faTail ++ (fbHead +: fbTail))
              case _ => err
            }
          case Success(a) =>
            fb match {
              case Success(b)      => Success(f(a, b))
              case err: Failure[_] => err
            }
        }

    }

  implicit val idApplicative: Applicative[Id] = // for EXERCISE 12.14
    new Applicative[Id] {
      def unit[A](a: => A): Id[A] = Id(a)

      override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] =
        Id(fab.value(fa.value))

      override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] =
        Id(
          f(fa.value, fb.value)
        )

    }

  type Const[A, B] = A

  @annotation.nowarn // книжный вариант
  implicit def monoidApplicative[M](monoid: Monoid[M]) =
    new Applicative[({ type ConstM[x] = Const[M, x] })#ConstM] {
      def unit[A](a: => A): M = monoid.zero

      override def apply[A, B](m1: M)(m2: M): M = monoid.op(m1, m2)

    }

  /** Посколку конструктор типа Const[M, A],
    *
    * игнорирует свой второй аргумент типа(A)
    *
    * то выражение Const[M, A] == M таким образом выражение
    * monoidApplicative_Explanation можно заменить на monoidApplicative
    */
  def monoidApplicative_Explanation[M](monoid: Monoid[M]) =
    new Applicative[({ type ConstM[x] = Const[M, x] })#ConstM] {
      def unit[A](a: => A): Const[M, A] = monoid.zero

      override def apply[A, B](m1: Const[M, A => B])(
          m2: Const[M, A]
      ): Const[M, B] = monoid.op(m1, m2)

    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  // it's implemented in terms each other for example purposes
  // in the real traverse instances we should implement the traverse func
  // def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
  //   sequence(map(fa)(f))

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  /** EXERCISE 12.14 Hard:
    *
    * Implement map in terms of traverse as a method on Traverse[F]. This estab-
    * lishes that Traverse is an extension of Functor and that the traverse
    * function is a generalization of map (for this reason we sometimes call
    * these traversable functors). Note that in implementing map, you can call
    * traverse with your choice of Applicative[G].
    * {{{
    *  trait Traverse[F[_]] extends Functor[F] {
    *   ...
    *  }
    * }}}
    */

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    import Applicative._
    traverse[Id, A, B](fa) { a =>
      val idAppl: Applicative[Id] = implicitly[Applicative[Id]]
      idAppl.unit(f(a))
      // or just :-) :
      // Id(f(a))
    }.value
  }

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
    import Applicative._
    traverse[({ type f[x] = Const[B, x] })#f, A, B](as)(f)(
      monoidApplicative(
        mb // здесь преобразование monoidApplicative(mb) для демонстрации
      )
    )
    // можно упростить до
    // traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](as)(f)(mb)

  }

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex_V1[A](ta: F[A]): F[(A, Int)] = {
    import StateUtil._
    traverseS(ta)((a: A) =>
      (for {
        i <- get[Int]
        _ <- set(i + 1)
      } yield (a, i))
    ).run(0)._1
  }

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = {
    import StateUtil._
    traverseS(fa)((a: A) =>
      (for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- StateUtil.set(s2)
      } yield b)
    ).run(s)
  }

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  /** EXERCISE 12.16
    *
    * There’s an interesting consequence of being able to turn any traversable
    * functor into a reversed list—we can write, once and for all, a function to
    * reverse any traversable functor! Write this function, and think about what
    * it means for List, Tree, and other traversable functors.
    */
  def reverse[A](fa: F[A]): F[A] =
    traverse[Id, A, A](fa)(Id.apply).value

  def reverse_BookV[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, acc) => (acc.head, acc.tail))._1

  /** EXERCISE 12.17
    *
    * Use mapAccum to give a default implementation of foldLeft for the Traverse
    * trait.
    */
  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z) { case (a: A, acc: B) =>
      val b = f(acc, a)
      () -> b
    }._2

  override def foldRight[A, B](fa: F[A])(z: B)(f: (A, B) => B): B =
    foldLeft(reverse(fa))(z) { case (b, a) =>
      f(a, b)
    }

  /** EXERCISE 12.18
    *
    * Use applicative functor products to write the fusion of two traversals.
    * This function will, given two functions f and g, traverse fa a single
    * time, collecting the results of both functions at once.
    */
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit
      gApplicative: Applicative[G],
      hApplicative: Applicative[H]
  ): (G[F[B]], H[F[B]]) = {

    type Product[X] = (G[X], H[X])

    val gAndHproductApplicative: Applicative[Product] =
      gApplicative.product(hApplicative)

    val ret: Product[F[B]] =
      traverse[Product, A, B](fa) { case a =>
        (f(a), g(a))
      }(gAndHproductApplicative)

    ret
  }

  /** EXERCISE 12.19
    *
    * Implement the composition of two Traverse instances.
    */
  def compose[G[_]](implicit
      gTraverse: Traverse[G]
  ): Traverse[({ type FG[x] = F[G[x]] })#FG] = {
    val self = this
    new Traverse[({ type FG[x] = F[G[x]] })#FG] {
      override def traverse[M[_]: Applicative, A, B](fa: F[G[A]])(
          f: A => M[B]
      ): M[F[G[B]]] = {
        self.traverse(fa) { ga: G[A] =>
          gTraverse.traverse(ga)(f)
        }
      }
    }
  }

}

object Traverse {

  /** EXERCISE 12.13
    *
    * Write Traverse instances for List, Option, and Tree.
    */
  @annotation.nowarn // for education purpose
  val listTraverse: Traverse[List] =
    new Traverse[List] {

      override def traverse[G[_]: Applicative, A, B](fa: List[A])(
          f: A => G[B]
      ): G[List[B]] = {
        val applicativeG: Applicative[G] = implicitly[Applicative[G]]
        fa.foldRight(applicativeG.unit(List.empty[B])) {
          case (elem: A, acc: G[List[B]]) =>
            applicativeG.map2(acc, f(elem)) { case (list, b) =>
              list :+ b
            }
        }
      }

      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
        as.foldRight(z)(f)

    }

  val optionTraverse =
    new Traverse[Option] {
      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
        as.map { a: A =>
          f(a, z)
        }.getOrElse(z)

      override def traverse[G[_]: Applicative, A, B](
          fa: Option[A]
      )(f: A => G[B]): G[Option[B]] = {

        val applicativeG = implicitly[Applicative[G]]
        fa match {
          case None           => applicativeG.unit(Option.empty[B])
          case Some(value: A) => applicativeG.map(f(value))(Option.apply)

        }
      }
    }

  // case class Tree[+A](head: A, tail: List[Tree[A]])
  val treeTraverse: Traverse[applicative.Tree] =
    new Traverse[Tree] {

      override def foldRight[A, B](tree: Tree[A])(z: B)(f: (A, B) => B): B =
        tree match {
          case Tree(head, Nil) => f(head, z)
          case Tree(head, second :: rest) =>
            f(
              head,
              foldRight[A, B](
                Tree(head = second.head, tail = second.tail ++ rest)
              )(z)(f)
            )
        }

      override def traverse[G[_]: Applicative, A, B](
          fa: Tree[A]
      )( // TODO:simplify via listTraverse
          f: A => G[B]
      ): G[Tree[B]] = {
        val applicativeG: Applicative[G] = implicitly[Applicative[G]]
        fa match {
          case Tree(head, Nil) =>
            applicativeG.map(f(head))(b => Tree(b, List.empty[Tree[B]]))

          case Tree(head, second :: rest) =>
            val headB: G[B] = f(head)
            val newTree = Tree(second.head, second.tail ++ rest)
            applicativeG.map2(headB, traverse(newTree)(f)) {
              case (b, tree: Tree[B]) =>
                Tree(
                  head = b,
                  tail = Tree(tree.head, List.empty[Tree[B]]) +: tree.tail
                )
            }
        }
      }

    }
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

  // r4.foreach(println)

  object exercise_127 {

    /** EXERCISE 12.7 Hard:
      *
      * Prove that all monads are applicative functors by showing that if the
      * monad laws hold, the Monad implementations of map2 and map satisfy the
      * applicative laws.
      */

    ??? // TODO:impl
  }

}
