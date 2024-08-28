package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import fpinscala.state._
import parallelism.Par._
import language.higherKinds
import java.lang
import fpinscala.monads.Monad.optionMonad
import fpinscala.iomonad.IO2aTests.f

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

  /** assuming that flatMap obeys an associative law:
    * {{{
    *   x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
    * }}}
    */
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

  /** EXERCISE 11.6 Hard:
    *
    * Here’s an example of a function we haven’t seen before. Implement the
    * function filterM. It’s a bit like filter, except that instead of a
    * function from A => Boolean, we have an A => F[Boolean]. (Replacing various
    * ordinary functions like this with the monadic equivalent often yields
    * interesting results.) Implement this function, and then think about what
    * it means for various data types.
    */
  @annotation.nowarn // unchecked suppressed for for educational purposes
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldLeft(unit(List.empty[A])) {
      //
      case (ml: M[List[A]], a: A) =>
        val mb: M[Boolean] = f(a)
        println("a:" + a)
        println("ml:" + ml)
        println("mb:" + mb)
        val stepRet = flatMap(ml) { accList: List[A] =>
          println("acc list" + accList)
          val mapMB: M[List[A]] = map(mb) {
            case true  => accList :+ a
            case false => accList
          }
          println("map mb:" + mapMB)
          mapMB
        }

        println("step ret:" + stepRet)
        println("-------------")
        stepRet
    }

  def filterM_Book[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List[A]()))((x, y) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x)
    )

  /** EXERCISE 11.7
    *
    * functions like A => M[B] are called Kleisli arrows
    *
    * Implement the Kleisli composition function compose.
    *
    * compose(compose(f, g), h) == compose(f, compose(g, h))
    */

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a =>
      flatMap(f(a)) { b =>
        g(b)
      }

  /** EXERCISE 11.8 Hard:
    *
    * Implement flatMap in terms of compose. It seems that we’ve found another
    * minimal set of monad combinators: compose and unit.
    */
  // A` == A == B, B` == C
  def flatMap_via_compose[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose[A, A, B](
      f = _ => ma,
      g = f
    )(null.asInstanceOf[A])

  // sic!
  def flatMap_via_compose_book_version[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose[Unit, A, B]((_: Unit) => ma, f)(())

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

  val idMonad: Monad[Id] =
    new Monad[Id] {
      def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
      def unit[A](a: => A): Id[A] = Id(a)
    }

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

object examples_monadic extends App {
  import scala.concurrent._
  import scala.concurrent.duration._

  implicit val ec: scala.concurrent.ExecutionContextExecutor =
    scala.concurrent.ExecutionContext.global

  /** EXERCISE 11.5
    *
    * Think about how replicateM will behave for various choices of F. For
    * example, how does it behave in the List monad? What about Option? Describe
    * in your own words the general meaning of replicateM.
    */

  def futureMonad = {
    val futureM: Monad[Future] =
      new Monad[Future] {
        def flatMap[A, B](ma: Future[A])(f: A => Future[B]): Future[B] =
          ma.flatMap(f)
        def unit[A](a: => A): Future[A] = Future(a)
      }

    futureM
  }
  lazy val example_115: Unit = {

    val futureM = futureMonad
    // Если у нас есть к примеру Monad[Future] то replicaM(12,Future(42))
    // вернет список повторяющихся чисел 42 обернутый в Future
    val ret1: Future[List[Int]] = futureM.replicateM(12, Future(42))
    // Если у нас есть  Monad[Option] то replicaM(12,Option(42))
    // вернет список повторяющихся чисел 42 внутри Option
    val ret2: Option[List[Int]] = Monad.optionMonad.replicateM(12, Option(42))
    // Если у нас есть  Monad[List] то replicaM(12,List(1,2,3))
    // вернет список повторяющихся списков
    val ret3: List[List[Int]] = Monad.listMonad.replicateM(12, List(1, 2, 3))
  }

  object example_116 { // TODO: think about results
    object OptionMonadFilterM {
      val ret1: Option[List[Int]] =
        Monad.optionMonad.filterM(List(1, 2, 3))(x =>
          if (x > 1) Some(true) else None
        )

      // step 1
      // a =1 , ml = Option(List.empty)
      // mb = None, bcs (! 1  > 1)
      // return None
      // step 2
      // a =2 , ml = None
      // mb = Some(true), bcs (2 > 1)
      // return None
      // step 3
      // a = 3 , ml = None
      // mb = Some(true), bcs (3 > 1)
      // return None
      // RET = None

      val ret1_B: Option[List[Int]] =
        Monad.optionMonad.filterM_Book(List(1, 2, 3))(x =>
          if (x > 1) Some(true) else None
        )
      println("Option monad:" + ret1)
      println("Option monad (book version):" + ret1_B)
    }

    // OptionMonadFilterM

    val ret2: List[List[Int]] =
      Monad.listMonad.filterM(List(1, 2, 3))((x: Int) =>
        if (x > 1) List(true, false) else List(false)
      )

    val ret2_Book: List[List[Int]] =
      Monad.listMonad.filterM_Book(List(1, 2, 3))((x: Int) =>
        if (x > 1) List(true, false) else List(false)
      )
    println("List monad:" + ret2)
    println("List monad (book version):" + ret2_Book)

    // val ret3: Future[List[Int]] =
    //   futureMonad.filterM(List(1, 2, 3))(a =>
    //     Future {
    //       if (a > 1) {
    //         true
    //       } else {
    //         false
    //       }
    //     }
    //   )

    // val ret3_Book: Future[List[Int]] =
    //   futureMonad.filterM_Book(List(1, 2, 3))(a =>
    //     Future {
    //       if (a > 1) {
    //         true
    //       } else {
    //         false
    //       }
    //     }
    //   )

    // val ret33: Future[Unit] =
    //   for {
    //     f1 <- ret3
    //     f2 <- ret3_Book
    //   } yield {
    //     println(f1)
    //     println(f2)
    //   }

    // Await.ready(ret33, 1.second)

  }

  example_116

}
