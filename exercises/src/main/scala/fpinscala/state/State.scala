package fpinscala.state

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import fpinscala.state
import scala.annotation.tailrec
import java.util.concurrent.Flow

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  def between(low: Int, high: Int): (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }

    // WARN: this method is slow
    final def between(low: Int, high: Int): (Int, RNG) = {

      @tailrec
      def loop(rnd: RNG): (Int, RNG) = {
        val (v, nextRNG) = rnd.nextInt
        if ((v >= low) && (v < high)) {
          v -> nextRNG
        } else {
          loop(nextRNG)
        }
      }

      loop(this)

    }

  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /** EXERCISE 6.1
    *
    * Write a function that uses RNG.nextInt to generate a random integer
    * between 0 and Int.maxValue (inclusive). Make sure to handle the corner
    * case when nextInt returns Int.MinValue, which doesn’t have a non-negative
    * counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (halfRet, newRNG) = rng.nextInt
    halfRet match {
      case Int.MinValue => math.abs(halfRet + 1) -> newRNG
      case x            => math.abs(x) -> newRNG
    }
  }

  /** EXERCISE 6.2
    *
    * Write a function to generate a Double between 0 and 1, not including 1.
    * Note: You can use Int.MaxValue to obtain the maximum positive integer
    * value, and you can use x.toDouble to convert an x: Int to a Double.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (halfRet, newRNG) = nonNegativeInt(rng)
    halfRet match {
      case m @ Int.MaxValue => (m - 1).toDouble / m -> newRNG
      case x                => (x.toDouble / Int.MaxValue) -> newRNG
    }
  }

  /** EXERCISE 6.3
    *
    * Write functions to generate an (Int, Double) pair, a (Double, Int) pair,
    * and a (Double, Double, Double) 3-tuple. You should be able to reuse the
    * functions you’ve already written.
    */

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rngN) = rng.nextInt
    val (d, rndR) = double(rngN)

    (i, d) -> rndR

  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {

    val (d, rndR) = double(rng)
    val (i, rngN) = rndR.nextInt

    (d, i) -> rndR
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rnd1) = double(rng)
    val (d2, rng2) = double(rnd1)
    val (d3, rng3) = double(rng2)

    (d1, d2, d3) -> rng3

  }

  /** EXERCISE 6.4
    *
    * Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val lb = ListBuffer.empty[Int]

    def loop(c: Int, rngN: RNG): RNG =
      if (c > 0) {
        val (i, nextRNG) = rngN.nextInt
        lb += i
        loop(c - 1, nextRNG)
      } else {
        rngN
      }

    val retRNG = loop(c = count, rngN = rng)
    lb.toList -> retRNG
  }

  /** EXERCISE 6.5
    *
    * Use map to reimplement double in a more elegant way. See exercise 6.2.
    */

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt) {
      case m @ Int.MaxValue => (m - 1).toDouble / m
      case x                => (x.toDouble / Int.MaxValue)
    }

  /** cit
    *
    * ```Unfortunately, map isn’t powerful enough to implement intDouble and
    * doubleInt from exercise 6.3. What we need is a new combinator map2 that
    * can combine two RNG actions into one using a binary rather than unary
    * function.
    * ```
    *
    * is it ugly ?
    */
  def intDouble: Rand[(Int, Double)] = {
    map(_.nextInt)(identity).andThen { case ((int, rng)) =>
      val (dbl, nRng) = double(rng = rng)
      (int -> dbl) -> nRng
    }
  }

  /** EXERCISE 6.6
    *
    * Write the implementation of map2 based on the following signature. This
    * function takes two actions, ra and rb, and a function f for combining
    * their results, and returns a new action that combines them:
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    map(ra)(identity).andThen { case ((a, rngN)) =>
      map(rb) { b: B =>
        f(a, b)
      }(rngN)
    }

  /** EXERCISE 6.7
    *
    * Hard: If you can combine two RNG transitions, you should be able to
    * combine a whole list of them. Implement sequence for combining a List of
    * transitions into a single transition. Use it to reimplement the ints
    * function you wrote before. For the latter, you can use the standard
    * library function List.fill(n)(x) to make a list with x repeated n times.
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    (rng: RNG) => {
      val lb = ListBuffer.empty[A]
      val nextRNG: RNG =
        fs.foldLeft(rng) { case (prev: RNG, rand: Rand[A]) =>
          val (a, next: RNG) = rand(prev)
          lb += a
          next
        }
      lb.toList -> nextRNG
    }

  /** EXERCISE 6.8
    *
    * Implement flatMap, and then use it to implement nonNegativeLessThan.
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    (rng: RNG) => {
      val (a, rng1) = f(rng)
      val ret: (B, RNG) = g(a)(rng1)
      ret
    }
  // TODO:test
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { x: Int =>
      val mod = x % n
      if (x + (n - 1) - mod >= 0)
        unit(mod)
      else {
        nonNegativeLessThan(n)
      }
    }

  def moreOrEqualThan(n: Int): Rand[Int] =
    flatMap(int) { x: Int =>
      if (x >= 0)
        unit(x)
      else {
        moreOrEqualThan(n)
      }
    }

  def between(low: Int, high: Int): Rand[Int] = _.between(low, high)

  /** EXERCISE 6.9
    *
    * Reimplement map and map2 in terms of flatMap. The fact that this is
    * possible is what we’re referring to when we say that flatMap is more
    * powerful than map and map2.
    */

  def mapV2[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2V2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a: A =>
      map(rb) { b =>
        f(a, b)
      }
    }

}

case class State[S, +A](run: S => (A, S)) {

  case class FlowState[AA >: A](a: AA, state: S) {
    def flow: FlowState[AA] = {
      // println(s"FlowState[a:$a,state:$state] before run ")
      val (aa, next) = run(state)
      // println(s"FlowState[aa:$aa,next:$next] after run ")
      FlowState(aa, next)
    }
  }

  import State._

  /** EXERCISE 6.10
    *
    * Generalize the functions unit, map, map2, flatMap, and sequence. Add them
    * as meth- ods on the State case class where possible. Otherwise you should
    * put them in a State companion object.
    */

  def map[B](f: A => B): State[S, B] = flatMap { a =>
    unit[S, B](f(a))
  }

  def flow[AA >: A](s: S): FlowState[AA] = {
    val (a, next) = run(s)
    FlowState[AA](a, next)
  }

  def mapAlt[B](f: A => B): State[S, B] =
    State[S, B] {
      run.andThen { case ((a, nextS)) =>
        f(a) -> nextS
      }
    }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a: A =>
      sb.map { b =>
        f(a, b)
      }
    }

  @annotation.nowarn // disabled for educaion purpose
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B](
      run.andThen { case ((a: A, nextState: S)) =>
        f(a).run(nextState)
      }
    )

  def modifyMy(f: S => S): State[S, A] =
    State { s: S =>
      run(f(s))
    }

  def getMy[S]: State[S, S] =
    State { s: S =>
      s -> s
    }

  def setMy(s: S): State[S, A] =
    State { _: S =>
      run(s)
    }

}

object State {

  def unit[S, R](r: R): State[S, R] = State((s: S) => (r, s))

  def sequence[S, A](xa: List[State[S, A]]): State[S, List[A]] =
    xa.foldLeft(unit[S, List[A]](List.empty[A])) {
      case (acc: State[S, List[A]], elem: State[S, A]) =>
        acc.flatMap { accList =>
          elem.map(a => accList :+ a)
        }
    }

  type Rand[A] = State[RNG, A]

  def modifyB[S](f: S => S): State[S, Unit] = for {
    s <- getB // Gets the current state and assigns it to `s`.
    _ <- setB(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def getB[S]: State[S, S] = State(s => (s, s))

  def setB[S](s: S): State[S, Unit] = State(_ => ((), s))

  /** EXERCISE 6.11
    *
    * Hard: To gain experience with the use of State, implement a finite state
    * automaton that models a simple candy dispenser. The machine has two types
    * of input: you can insert a coin, or you can turn the knob to dispense
    * candy. It can be in one of two states: locked or unlocked. It also tracks
    * how many candies are left and how many coins it contains.
    */

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def tryLock: Machine = copy(
      locked = candies <= 0 || coins <= 0
    )

    def isOpen = !locked

    // конфетки можно добавлять сколько угодно
    def addCandies(c: Int) = copy(candies = candies + c).tryUnlock

    // монетки можно добавляеть не больше запаса конфет в машине
    def addCoins(c: Int) =
      (if (coins + c <= candies) {
         copy(coins = coins + c)
       } else {
         this
       }).tryUnlock

    def tryUnlock = copy(
      locked = !canOpen
    )

    def canOpen = candies > 0 && coins > 0

  }

  object Machine {
    def empty = Machine(locked = true, candies = 0, coins = 0)
  }

  def set(machine: Machine, input: Input): Machine =
    input match {
      case Coin => machine.addCoins(1)
      case Turn if machine.isOpen =>
        val newCandies =
          if (machine.candies - 1 >= 0) {
            machine.candies - 1
          } else {
            machine.candies
          }

        val newMachine = machine.copy(candies = newCandies)
        newMachine.tryLock

      case Turn =>
        println(s"Warn: The Machine[$machine] is locked")
        machine

    }

  type CandyMachineState = State[Machine, (Int, Int)]

  // возвращает конфеты -> монеты и новый state
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State { intialState: Machine =>
      val nextState: Machine =
        inputs.foldLeft(intialState) { case (machine, in) =>
          set(machine, in)
        }

      (nextState.candies -> nextState.coins) -> nextState
    }

  def simulateMachineBookV(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map { in =>
        // эта функция берет текущий стайт и изменяет его через S => S
        val modifyF: (Machine => Machine) => State[Machine, Unit] =
          modifyB[Machine]

        // сначала выполнится update и вернет функцию Machine => Machine
        // затем ее возмет modifyF как аргумент и вернет State[Machine, Unit]
        val f: Input => State[Machine, Unit] = modifyF compose update
        val item: State[Machine, Unit] = f(in)
        item

      })
      s <- getB
    } yield (s.coins, s.candies)

  def update = (i: Input) =>
    (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _))        => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _))  => s
        case (Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
      }

}

object example_flow_state extends App {

  val state: State[Long, String] = State((l: Long) => (l.toString, l + 1))

  val fs: state.FlowState[String] = state.flow(0L).flow.flow.flow
  println(fs)

}
