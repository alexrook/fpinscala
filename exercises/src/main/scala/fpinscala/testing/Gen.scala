package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

trait Prop {}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A, B](f: A => B): Gen[B] = ???
  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {}

/** EXERCISE 8.1 
 * 
 * To get used to thinking about testing in this way, come up with
  * properties that specify the implementation of a sum: List[Int] => Int
  * function. You don’t have to write your properties down as executable
  * ScalaCheck code—an informal description is fine. Here are some ideas to get
  * you started: 
    - Reversing a list and summing it should give the same result
  * as summing the original, nonreversed list. 
  * - What should the sum be if all
  * elements of the list are the same value? 
  * - Can you think of other properties
  */

  //ns.reverse.sum == ns.sum
  //List.fill(10)(3).sum == 10*3
  //List.empty[Number].sum == 0
  //List(n, n+1, n+2,.. k) == lenght * (n + k) / 2
