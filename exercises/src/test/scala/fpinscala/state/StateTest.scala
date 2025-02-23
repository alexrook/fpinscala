package fpinscala.state

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Assertions._
import org.scalatest.matchers.should.Matchers._
import fpinscala.parallelism.Par

import fpinscala.state.State.Machine

class StateTest extends AnyWordSpec {

  import fpinscala.state.RNG
  import fpinscala.state.RNG._

  "RNG" when {

    "between method called" should {

      "return correct values" in {
        val rng = RNG.Simple(1000L)

        // val r1: Rand[Int] = between(1, 2)
        // val (actualR1, actualRNG1) = r1(rng)
        // println(actualR1)

        val r2: Rand[Int] = between(1000, 2000)
        val (actualR2, actualRNG2) = r2(rng)
        println(actualR2)

      }
    }

    "simulateMachine method called " should {
      import fpinscala.state.State._

      "return correct values" in {
        val inputs1 =
          List(
            Coin,
            Turn,
            Coin,
            Coin
          )

        val m1: State[State.Machine, (Int, Int)] = simulateMachine(inputs1)

        val (m1r1R, m1r1S) = m1.run(Machine.empty)
        m1r1R shouldBe (0, 0) // нет монеток и  без конфет

        val (m1r2R, m1r2S) = m1.run(Machine.empty.addCandies(3))
        m1r2R shouldBe (2, 2) // две монетки две конфеты

        val (m1r3R, m1r3S) =
          m1.run(Machine(locked = false, candies = 10, coins = 5))
        m1r3R shouldBe (9, 8)

        val inputs2 = List.fill(4)(Turn)
        val m2: State[State.Machine, (Int, Int)] = simulateMachine(inputs2)
        val (m2r2R, m2r2S) =
          m2.run(Machine(locked = false, candies = 10, coins = 5))
        m2r2R shouldBe (6, 5) // четыре конфеты купили (10 - 4 == 6)

      }
    }

    "mapV2 method called " should {
      "return correct values" in {

        val rng = RNG.Simple(42)

        val rand: Rand[Int] = mapV2(nonNegativeInt)(identity)

        val (initialRet, initialRNG) = rand(rng)

        0 to 10 foreach { _ =>
          val (actualR, actualRNG) = rand(rng)
          assert(actualR == initialRet)
          assert(actualRNG == initialRNG)
        }
      }

      "return correct results for corner cases" in {

        def check(rng: RNG) {
          val (ret1, _) = mapV2(nonNegativeInt)(identity)(rng)
          assert(ret1 >= 0)
        }

        check(RNG.Simple(Int.MinValue))
        check(RNG.Simple(Int.MaxValue))
        check(RNG.Simple(0))
      }
    }

    "map2V2 method called " should {
      "return correct values" in {

        val rng = RNG.Simple(42)

        val rand: Rand[(Int, Double)] = map2V2(nonNegativeInt, double)(_ -> _)

        val (initialRet, initialRNG) = rand(rng)

        0 to 10 foreach { _ =>
          val (actualR, actualRNG) = rand(rng)
          assert(actualR == initialRet)
          assert(actualRNG == initialRNG)
        }
      }

      "return correct results for corner cases" in {

        def check(rng: RNG) {
          val ((ret1, dbl1), _) = map2V2(nonNegativeInt, double)(_ -> _)(rng)
          assert(dbl1 >= 0 && dbl1 < 1)
          assert(ret1 >= 0)
        }

        check(RNG.Simple(Int.MinValue))
        check(RNG.Simple(Int.MaxValue))
        check(RNG.Simple(0))
      }
    }

    "flatMap method called" should {

      def check[A](rng: RNG, rand: Rand[A]) = {
        val (initialRet, initialRNG) = rand(rng)

        0 to 10 foreach { _ =>
          val (actualR, actualRNG) = rand(rng)
          assert(actualR == initialRet)
          assert(actualRNG == initialRNG)
        }
      }
      "return correct results" in {
        val rng = RNG.Simple(42)

        val rand: Rand[Int] =
          flatMap(nonNegativeInt)(unit)

        val (initialRet, initialRNG) = rand(rng)

        0 to 10 foreach { _ =>
          val (actualR, actualRNG) = rand(rng)
          assert(actualR == initialRet)
          assert(actualRNG == initialRNG)
        }
      }

      "return correct results for corner cases" in {
        val rand: Rand[Int] = flatMap(nonNegativeInt)(unit)

        check(RNG.Simple(Int.MinValue), rand)
        check(RNG.Simple(Int.MaxValue), rand)
        check(RNG.Simple(0), rand)
      }

    }

    "sequence method called" should {

      def check(rng: RNG, list: List[Rand[Int]]) = {
        val actual = sequence(list)
        val (retL: List[Int], retRNG) = actual(rng)
        if (list.isEmpty) {
          retRNG shouldEqual rng
          retL shouldBe empty
        } else {
          retRNG should not equal rng
          retL.size shouldEqual list.size
        }

      }
      "return correct results" in {
        val list: List[Rand[Int]] = List.fill(7)(nonNegativeInt _)
        val rng = RNG.Simple(42)
        check(rng, list)
      }

      "return correct results for corner cases" in {
        val list: List[Rand[Int]] = List.fill(7)(nonNegativeInt _)
        check(RNG.Simple(Int.MinValue), list)
        check(RNG.Simple(Int.MaxValue), list)
        check(RNG.Simple(0), list)
      }
    }

    "map2 method called " should {
      "return correct values" in {

        val rng = RNG.Simple(42)

        val rand: Rand[(Int, Double)] = map2(nonNegativeInt, double)(_ -> _)

        val (initialRet, initialRNG) = rand(rng)

        0 to 10 foreach { _ =>
          val (actualR, actualRNG) = rand(rng)
          assert(actualR == initialRet)
          assert(actualRNG == initialRNG)
        }
      }

      "return correct results for corner cases" in {

        def check(rng: RNG) {
          val ((ret1, dbl1), _) = map2(nonNegativeInt, double)(_ -> _)(rng)
          assert(dbl1 >= 0 && dbl1 < 1)
          assert(ret1 >= 0)
        }

        check(RNG.Simple(Int.MinValue))
        check(RNG.Simple(Int.MaxValue))
        check(RNG.Simple(0))
      }
    }

    "doubleViaMap method called " should {
      "return values between 0 and 1 multiple times" in {
        for (x <- -10 to 10) {
          val rng = RNG.Simple(x)
          val (ret, _) = doubleViaMap(rng)
          assert(ret >= 0 && ret < 1)
        }
      }

      "return correct results for corner cases" in {
        val rng1 = RNG.Simple(Int.MinValue)
        val (ret1, _) = doubleViaMap(rng1)
        assert(ret1 >= 0 && ret1 < 1)

        val rng2 = RNG.Simple(Int.MaxValue)
        val (ret2, _) = doubleViaMap(rng2)
        assert(ret2 >= 0 && ret2 < 1)

        val rng3 = RNG.Simple(0)
        val (ret3, _) = doubleViaMap(rng3)
        assert(ret3 >= 0 && ret3 < 1)
      }
    }

    "ints method called" should {
      "return repetable results" in {
        val rng = RNG.Simple(42)
        val (initial, _) = ints(10)(rng)

        for (_ <- 0 to 10) {
          val (actual, _) = ints(10)(rng)
          actual should contain theSameElementsAs initial
        }

      }

      "return correct results for corner cases" in {
        val rng1 = RNG.Simple(Int.MinValue)
        val (ret1, _) = ints(10)(rng1)
        assert(ret1.size == 10)

        val rng2 = RNG.Simple(Int.MaxValue)
        val (ret2, _) = ints(10)(rng2)
        assert(ret2.size == 10)
      }
    }

    "nextInt method called " should {
      "return the same results multiple times" in {
        val rng = RNG.Simple(42)

        val r1 = (rng.nextInt._1)
        val r2 = (rng.nextInt._1)
        val r3 = (rng.nextInt._1)

        assert(r1 == r2 && r2 == r3)

      }
    }

    "nonNegativeInt method called " should {
      "return positive results multiple times" in {
        for (x <- -10 to 10) {
          val rng = RNG.Simple(x)
          val (ret, _) = nonNegativeInt(rng)
          assert(ret > -1)
        }

      }

      "return positive results for corner cases" in {
        val rng1 = RNG.Simple(Int.MinValue)
        val (ret1, _) = nonNegativeInt(rng1)
        assert(ret1 > -1)

        val rng2 = RNG.Simple(Int.MaxValue)
        val (ret2, _) = nonNegativeInt(rng2)
        assert(ret2 > -1)
      }
    }

    "double method called " should {
      "return values between 0 and 1 multiple times" in {
        for (x <- -10 to 10) {
          val rng = RNG.Simple(x)
          val (ret, _) = double(rng)
          assert(ret >= 0 && ret < 1)
        }
      }

      "return correct results for corner cases" in {
        val rng1 = RNG.Simple(Int.MinValue)
        val (ret1, _) = double(rng1)
        assert(ret1 >= 0 && ret1 < 1)

        val rng2 = RNG.Simple(Int.MaxValue)
        val (ret2, _) = double(rng2)
        assert(ret2 >= 0 && ret2 < 1)

        val rng3 = RNG.Simple(0)
        val (ret3, _) = double(rng3)
        assert(ret3 >= 0 && ret3 < 1)
      }
    }
  }
}
