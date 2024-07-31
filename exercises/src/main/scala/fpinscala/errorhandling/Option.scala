package fpinscala.errorhandling

import scala.{
  Option => _,
  Some => _,
  Either => _,
  _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import fpinscala.streamingio.GeneralizedStreamTransducers.Process.id
import scala.collection.mutable.ListBuffer

/** EXERCISE 4.1 Implement all of the functions on Option. As you implement each
  * function, try to think about what it means and in what situations you’d use
  * it. We’ll explore when to use each of these functions next. Here are a few
  * hints for solving this exercise: It’s fine to use pattern matching, though
  * you should be able to implement all the functions besides map and getOrElse
  * without resorting to pattern matching. For map and flatMap, the type
  * signature should be enough to determine the implementation. getOrElse
  * returns the result inside the Some case of the Option, or if the Option is
  * None, returns the given default value. orElse returns the first Option if
  * it’s defined; otherwise, it returns the second Option.
  */

sealed trait Option[+A] { self =>

  def flatten[B](implicit ev: A <:< Option[B]): Option[B] =
    self match {
      case None    => None
      case Some(a) => ev(a)
    }

  def map[B](f: A => B): Option[B] =
    self match {
      case Some(a) => Some(f(a))
      case None    => None
    }

  def getOrElse[B >: A](default: => B): B =
    self match {
      case Some(v) => v
      case None    => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).flatten

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some.apply).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap {
      case a if f(a) => Some(a)
      case _         => None
    }

  final def foreach[U](f: A => U): Unit = map(f)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def apply[A](v: A): Option[A] =
    if (v == null) {
      None
    } else {
      Some(v)
    }

  def failingFn(i: Int): Int = {
    val y: Int =
      throw new Exception(
        "fail!"
      ) // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception(
        "fail!"
      )): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

//   EXERCISE 4.2
// Implement the variance function in terms of flatMap. If the mean of a sequence is m,
// the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
// See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
// def variance(xs: Seq[Double]): Option[Double]
  def variance(xs: Seq[Double]): Option[Double] = {

    def mean(f: Double => Double): Option[Double] =
      xs.foldLeft((0.0d, 0)) { case ((a, c), x) =>
        (a + f(x)) -> (c + 1)
      } match {
        case (a, c) if c > 0 => Some(a / c)
        case _               => None
      }

    for {
      m <- mean(identity)
      // _ = println(s"Mean[$m]")
      ret <- mean { x: Double =>
        math.pow(x - m, 2)
      }
      // _ = println(s"Variance[$ret]")
    } yield ret

  }

//   EXERCISE 4.3
// Write a generic function map2 that combines two Option values using
// a binary function.
//If either Option value is None, then the return value is too.
//Here is its signature:
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap { a =>
      b.map { b =>
        f(a, b)
      }
    }

  /** EXERCISE 4.4
    *
    * Write a function sequence that combines a list of Options into one Option
    * containing a list of all the Some values in the original list. If the
    * original list contains None even once, the result of the function should
    * be None; otherwise the result should be Some with a list of all the
    * values. Here is its signature:3
    */
  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    if (xs.isEmpty) {
      None
    } else {
      xs.foldLeft(Some(List.empty[A]): Option[List[A]]) {
        case (Some(list), Some(a)) =>
          Some(list :+ a)
        case _ => None
      }
    }

  /** EXERCISE 4.5 Implement this function. It’s straightforward to do using map
    * and sequence, but try for a more efficient implementation that only looks
    * at the list once. In fact, implement sequence in terms of traverse.
    */
  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] =
    xs.foldLeft(Some(ListBuffer.empty[B]): Option[ListBuffer[B]]) {
      case (accOpt, a) =>
        for {
          acc <- accOpt
          r <- f(a)
        } yield acc += r
    }.filter(_.nonEmpty)
      .map(_.toList)

//In fact, implement sequence in terms of traverse.
  def sequence2[A](xs: List[Option[A]]): Option[List[A]] =
    traverse(xs)(identity)

}
