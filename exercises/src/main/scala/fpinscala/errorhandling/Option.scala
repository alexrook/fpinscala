package fpinscala.errorhandling

import scala.{
  Option => _,
  Some => _,
  Either => _,
  _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

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
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
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
  def variance(xs: Seq[Double]): Option[Double] = ???

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}
