package fpinscala.state

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Assertions._
import org.scalatest.matchers.should.Matchers._
import fpinscala.parallelism.Par

class StateTest extends AnyWordSpec {

  import fpinscala.state.RNG
  import fpinscala.state.RNG._

  "RNG" when {

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
