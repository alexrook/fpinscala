package fpinscala.laziness

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Assertions._
import org.scalatest.matchers.should.Matchers._
import fpinscala.parallelism.Par

class StreamTest extends AnyWordSpec {

  import fpinscala.laziness.Stream
  import fpinscala.laziness.Stream._

  "Custom Stream" when {

    "toList method called " should {
      "return correct results for non empty list" in {
        Stream(1).toList shouldBe List(1)
        Stream(1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
      }

      "return correct results for empty list" in {
        Stream.empty[Int].toList shouldBe List.empty[Int]
      }

    }

  }

}
