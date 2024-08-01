package fpinscala.errorhandling

import scala.{
  Option => _,
  Either => _,
  Left => _,
  Right => _,
  _
} // hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.collection.mutable.ListBuffer

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

  def mapLeft[E1](fL: E => E1): Either[E1, A] =
    fold[Either[E1, A]](Right.apply)(fL.andThen(Left.apply))

  def isLeft: Boolean =
    fold(_ => false)(_ => true)

  def isRight: Boolean = !isLeft

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

  def right[E, A](a: A): Either[E, A] = Right(a)

  /** EXERCISE 4.7 Implement sequence and traverse for Either. These should
    * return the first error that’s encountered, if there is one.
    *
    * TODO: there is no any cons[E], so traverse and sequence shouldn't fail for
    * empty List's and return Right(List.empty[B])?
    */
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldLeft(right[E, ListBuffer[B]](ListBuffer.empty[B])) {
      case (accE, a) =>
        for {
          acc <- accE
          r <- f(a)
        } yield acc += r
    }.map(_.toList)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

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

// EXERCISE 4.8 TODO
// In this implementation, map2 is only able to report one error, even if both the name
// and the age are invalid. What would you need to change in order to report both errors?
// Would you change map2 or the signature of mkPerson? Or could you create a new data
// type that captures this requirement better than Either does, with some additional
// structure? How would orElse, traverse, and sequence behave differently for that
// data type?

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson1(name: String, age: Int): Either[String, Person] =
    for {
      n <- mkName(name)
      a <- mkAge(age)
    } yield Person(n, a)

  object Validator {

    def sum[E, A](acc: Either[List[E], List[A]], next: Either[E, A]) =
      next match {
        case Left(e) if acc.isLeft =>
          acc.mapLeft(_ :+ e)

        case Left(e) =>
          Left(List.empty[E]).mapLeft(_ :+ e)

        case Right(a) =>
          acc.map(_ :+ a)
      }

    def traverse[E, A, B](es: List[A])(
        f: A => Either[E, B]
    ): Either[List[E], List[B]] =
      es.foldLeft(right[List[E], List[B]](List.empty[B])) { (acc, a) =>
        sum(acc, f(a))
      }

    def sequence[E, A](es: List[Either[E, A]]): Either[List[E], List[A]] =
      traverse(es)(identity)

  }

}
