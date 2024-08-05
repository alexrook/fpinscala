package fpinscala.laziness

import Stream._
import scala.collection.mutable.ListBuffer
import fpinscala.datastructures.List.tail
import scala.annotation.tailrec
trait Stream[+A] {

  /** EXERCISE 5.1
    *
    * Write a function to convert a Stream to a List, which will force its
    * evaluation and let you look at it in the REPL. You can convert to the
    * regular List type in the standard library. You can place this and other
    * functions that operate on a Stream inside the Stream trait. def toList:
    * List[A]
    */

  def toList: List[A] = {

    @tailrec
    def loop(acc: ListBuffer[A], stream: () => Stream[A]): List[A] =
      stream() match {
        case Cons(head, tail) => loop(acc += head(), tail)
        case Empty            => acc.toList
      }

    loop(ListBuffer.empty[A], () => this)
  }

  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /** EXERCISE 5.2
    *
    * Write the function take(n) for returning the first n elements of a Stream,
    * and drop(n) for skipping the first n elements of a Stream.
    */
  def take(n: Int): Stream[A] = {

    def loop(stream: Stream[A], a: Int): Stream[A] =
      stream match {
        case Cons(h, t) if a > 0 =>
          Stream.cons(h(), loop(t(), a = a - 1))
        case _ => Stream.empty[A]
      }

    loop(this, n)

  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def loop(stream: Stream[A], a: Int): Stream[A] =
      stream match {
        case Cons(_, _) if a <= 0 => stream
        case Cons(_, t)           => loop(t(), a - 1)
        case _                    => Stream.empty[A]
      }

    loop(this, n)
  }

  /** EXERCISE 5.3
    *
    * Write the function takeWhile for returning all starting elements of a
    * Stream that match the given predicate. def takeWhile(p: A => Boolean):
    * Stream[A]
    */
  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(stream: Stream[A]): Stream[A] =
      stream match {
        case Cons(h, t) =>
          val v = h() // one time execution
          if (p(v)) {
            Stream.cons(v, loop(t()))
          } else {
            Stream.empty[A]
          }

        case _ => Stream.empty[A]
      }

    loop(this)
  }

  /** EXERCISE 5.4
    *
    * Implement forAll, which checks that all elements in the Stream match a
    * given predi- cate. Your implementation should terminate the traversal as
    * soon as it encounters a nonmatching value.
    */
  def forAll(p: A => Boolean): Boolean = // TODO:replace with stack trace
    foldRight(false) { case (a, rest) =>
      p(a) || rest
    }

  /** EXERCISE 5.5
    *
    * Use foldRight to implement takeWhile.
    */
  def takeWhile2(p: A => Boolean): Stream[A] = // TODO:replace with stack trace
    foldRight(Stream.empty[A]) {
      case (el: A, b: Stream[A]) if p(el) =>
        Stream.cons(el, b)

      case _ => Stream.empty[A]
    }

  /** EXERCISE 5.6
    *
    * Hard: Implement headOption using foldRight.
    */
  def headOption: Option[A] =
    foldRight(Option.empty[A]) { case (a, _) =>
      Some(a)
    }

  /** EXERCISE 5.7 Implement map, filter, append, and flatMap using foldRight.
    * The append method should be non-strict in its argument.
    */

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B]) { case (a, b) =>
      Stream.cons[B](f(a), b)
    }

  // def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}
