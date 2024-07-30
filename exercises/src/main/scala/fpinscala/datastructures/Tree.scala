package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /** EXERCISE 3.25: Write a function size that counts the number of nodes
    * (leaves and branches) in a tree.
    */
  def size(tree: Tree[_]): Int =
    tree match {
      case Leaf(value)         => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  /** EXERCISE 3.26: Write a function maximum that returns the maximum element
    * in a Tree[Int]. (Note: In Scala, you can use x.max(y) or x max y to
    * compute the maximum of two integers x and y.)
    */
  def maximum[T](tree: Tree[T])(implicit ev: Ordering[T]): T = {
    import Ordered._

    tree match {
      case Leaf(value) => value
      case Branch(left, right) =>
        val leftM: T = maximum(left)
        val rightM: T = maximum(right)
        if (leftM >= rightM) {
          leftM
        } else {
          rightM
        }
    }

  }

  /** EXERCISE 3.27 Write a function depth that returns the maximum path length
    * from the root of a tree to any leaf.
    */

  def depth(tree: Tree[_]): Int =
    tree match {
      case Leaf(value) => 1
      case Branch(left, right) =>
        val lD: Int = 1 + depth(left)
        val rD: Int = 1 + depth(right)
        math.max(lD, rD)
    }

  /** EXERCISE 3.28 Write a function map, analogous to the method of the same
    * name on List, that modi- fies each element in a tree with a given
    * function.
    */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(value) =>
        Leaf(f(value))
      case Branch(left, right) =>
        Branch(map(left)(f), map(right)(f))
    }

  /** EXERCISE 3.29: Generalize size, maximum, depth, and map, writing a new
    * function fold that abstracts over their similarities. Reimplement them in
    * terms of this more general function. Can you draw an analogy between this
    * fold function and the left and right folds for List
    */
  def fold[A, B](
      tree: Tree[A]
  )(whenLeaf: A => B, whenBranch: (Tree[A], Tree[A]) => B): B =
    tree match {
      case Leaf(value) => whenLeaf(value)
      case Branch(left, right) =>
        whenBranch(left, right)
    }

  def size2(tree: Tree[_]): Int =
    fold(tree)(
      whenLeaf = _ => 1,
      whenBranch = (left, right) => 1 + size2(left) + size2(right)
    )

  def maximum2[T](tree: Tree[T])(implicit ev: Ordering[T]): T = {
    import Ordered._
    fold(tree)(
      whenLeaf = identity,
      whenBranch = (left, right) => {
        val l = maximum2(left)
        val r = maximum2(right)
        if (l >= r) {
          l
        } else {
          r
        }
      }
    )
  }

  def depth2(tree: Tree[_]): Int =
    fold(tree)(
      whenLeaf = _ => 1,
      whenBranch = (left, right) => {
        val lD: Int = 1 + depth2(left)
        val rD: Int = 1 + depth2(right)
        math.max(lD, rD)
      }
    )

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(
      whenLeaf = a => Leaf(f(a)),
      whenBranch = (left, right) => Branch(map2(left)(f), map2(right)(f))
    )

  def fold3[A, B](tree: Tree[A])(lF: A => B)(bF: (B, B) => B): B =
    tree match {
      case Leaf(value) => lF(value)
      case Branch(left, right) =>
        val lRet: B = fold3(left)(lF)(bF)
        val rRet: B = fold3(right)(lF)(bF)
        bF(lRet, rRet)
    }

  def size3(tree: Tree[_]): Int =
    fold3(tree)(_ => 1)((a, b) => 1 + a + b)

  def maximum3[T](tree: Tree[T])(implicit ev: Ordering[T]): T = {
    import Ordered._
    fold3(tree)(identity)((a, b) =>
      if (a >= b) {
        a
      } else {
        b
      }
    )
  }

  def depth3(tree: Tree[_]): Int =
    fold3(tree)(_ => 1)((a, b) => math.max(1 + a, 1 + b))

  def map3[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold3(tree)(l => Leaf(f(l)): Tree[B]) { case (a, b) =>
      Branch(a, b): Tree[B]
    }

}
