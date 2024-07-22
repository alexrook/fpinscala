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

  def maximum2[T](tree: Tree[T])(implicit ev: Ordering[T]): T = ???

  def depth2(tree: Tree[_]): Int = ???

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = ???

}
