package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil         => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(_, tail) => tail
      case Nil           => throw new NoSuchElementException("tail of empty list")
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil           => throw new NoSuchElementException("head of empty list")
      case Cons(_, tail) => Cons(h, tail)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Cons(_, tail) if n > 0 => drop(tail, n - 1)
      case _                      => l
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _                           => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil           => throw new NoSuchElementException("init of empty list")
      case Cons(h, Nil)  => Nil
      case Cons(h, tail) => Cons(h, init(tail))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(xa: List[A], acc: B): B =
      xa match {
        case Nil => acc
        case Cons(head, tail) =>
          val r = f(acc, head)
          println(s"acc[$acc], head[$head], f[$r]")
          loop(tail, r)
      }

    loop(l, z)
  }

  // Write sum, product, and a function to compute the length of a list using foldLeft.
  def sum[A](l: List[A], f: (A, A) => A, z: A) = foldLeft(l, z)(f)

  def product[A](l: List[A], f: (A, A) => A, z: A) = foldLeft(l, z)(f)

  def length2[_](l: List[_]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  // Write a function that returns the reverse of a list (given List(1,2,3) it returns
  // List(3,2,1)). See if you can write it using a fold.
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A]) {
    case (acc: List[A], a) => Cons(a, acc)
  }

//Hard: Can you write foldLeft in terms of foldRight? How about the other way
// around? Implementing foldRight via foldLeft is useful because it lets us implement
// foldRight tail-recursively, which means it works
//even for large lists without overflow-
// ing the stack.
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    val rev = reverse(as)
    foldLeft(rev, z) {
      case (b, a) => f(a, b)
    }
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = ??? // TODO: foldLeft via foldRight

  // Implement append in terms of either foldLeft or foldRight.
  def appendLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a1, a2) {
      case (acc: List[A], elem) => Cons(elem, acc)
    }

  def map[A, B](l: List[A])(f: A => B): List[B] = ???
}
