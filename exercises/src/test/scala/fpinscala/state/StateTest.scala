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
    }
  }
}
