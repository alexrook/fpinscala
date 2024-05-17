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
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2) {
      case (acc: List[A], elem) => Cons(elem, acc)
    }

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2) {
      case (elem, acc: List[A]) => Cons(elem, acc)
    }

  // Hard: Write a function that concatenates a list of lists into a single list. Its runtime
  // should be linear in the total length of all lists. Try to use functions we have already
  // defined.
  def concatLists[A](xa: List[List[A]]): List[A] = // TODO: O(?)
    foldLeft(xa, Nil: List[A]) {
      case (acc, inner) =>
        appendViaFoldRight(acc, inner)
    }

  // EXERCISE 3.16
  // Write a function that transforms a list of integers by adding 1 to each element.
  // (Reminder: this should be a pure function that returns a new List!)
  def addOne(xa: List[Int]): List[Int] = map(xa)(_ + 1)

  // EXERCISE 3.17
  // Write a function that turns each value in a List[Double] into a String. You can use
  // the expression d.toString to convert some d: Double to a String.
  def doubleToString(xa: List[Double]): List[String] = map(xa)(_.toString())

  // EXERCISE 3.18
  // Write a function map that generalizes modifying each element in a list while maintaining the structure of the list
  def map[A, B](l: List[A])(f: A => B): List[B] = { // TODO: check the best answer
    @tailrec
    def loop(xa: List[A], acc: List[B]): List[B] =
      xa match {
        case Nil => acc
        case Cons(head, tail) =>
          loop(tail, Cons(f(head), acc))
      }
    loop(reverse(l), Nil: List[B])
  }

  // EXERCISE 3.19
  // Write a function filter that removes elements from a list unless they satisfy a given
  // predicate. Use it to remove all odd numbers from a List[Int].
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A]) {
      case (elem, acc) if f(elem) => Cons(elem, acc)
      case (_, acc)               => acc
    }

  // EXERCISE 3.21
  // Use flatMap to implement filter.
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as) {
      case elem if f(elem) => Cons(elem, Nil)
      case _               => Nil
    }

// Write a function flatMap that works like map except that the function given will return
// a list instead of a single result, and that list should be inserted into the final resulting
// list. For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B]) {
      case (elem, acc) =>
        append(f(elem), acc)
    }

  // EXERCISE 3.22
// Write a function that accepts two lists and constructs a new list by adding correspond-
// ing elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
  def addCorresponding(l: List[Int], r: List[Int]): List[Int] = { // TODO:check the best answer
    def loop(left: List[Int], right: List[Int], acc: List[Int]): List[Int] =
      left match {
        case Nil => acc
        case Cons(lh, lt) =>
          right match {
            case Nil => loop(lt, right, Cons(lh, acc))
            case Cons(rh, rt) =>
              loop(lt, rt, Cons(lh + rh, acc))
          }
      }
    reverse(loop(l, r, Nil: List[Int]))
  }

  // EXERCISE 3.23
  // Generalize the function you just wrote so that it’s not specific to integers or addition.
  // Name your generalized function zipWith.
  def zipWith[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] = { // TODO:check the best answer
    def loop(left: List[A], right: List[A], acc: List[A]): List[A] =
      left match {
        case Nil => acc
        case Cons(lh, lt) =>
          right match {
            case Nil => loop(lt, right, Cons(lh, acc))
            case Cons(rh, rt) =>
              loop(lt, rt, Cons(f(lh, rh), acc))
          }
      }
    reverse(loop(l, r, Nil: List[A]))
  }

}
