package fpinscala.errorhandling

import scala.{
  Option => _,
  Either => _,
  Left => _,
  Right => _,
  _
} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {

  def joinLeft[EE >: E, A1 >: A](implicit
      ev: E <:< Either[EE, A1]
  ): Either[EE, A1] =
    fold[Either[EE, A1]](a => Right(a))(e => ev(e))
  // this match {
  //   case Left(e)     => ev(e)
  //   case r: Right[A] => r
  // }

  def joinRight[EE >: E, A1 >: A](implicit
      ev: A <:< Either[EE, A1]
  ): Either[EE, A1] =
    fold(a => ev(a))(e => Left(e))
  // this match {
  //   case l: Left[E] => l
  //   case Right(r)   => ev(r)
  // }

  def fold[C](fR: A => C)(fL: E => C): C =
    this match {
      case Right(b) => fR(b)
      case Left(a)  => fL(a)
    }

  /** EXERCISE 4.6 Implement versions of map, flatMap, orElse, and map2 on
    * Either that operate on the Right value.
    */
  def map[B](f: A => B): Either[E, B] =
    flatMap(f.andThen(Right.apply))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    fold(f)(Left.apply)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    fold[Either[EE, B]](Right.apply)(_ => b)

  def map2[EE >: E, B, C](other: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap { a =>
      other.map { b =>
        f(a, b)
      }
    }

}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    ???

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
