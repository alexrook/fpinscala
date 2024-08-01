package fpinscala.errorhandling

import scala.{
  Option => _,
  Some => _,
  None => _,
  Either => _,
  Right => _,
  Left => _,
  _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Assertions._

class EitherTest extends AnyWordSpec {

  import fpinscala.errorhandling.Either._
  import fpinscala.errorhandling.Either

  "Custom Either" when {

    "fold method called " should {
      "return correct results" in {
        assert(
          Right(1).fold[Either[String, Int]](Right.apply)(identity) == Right(
            1
          )
        )

        assert(
          Left("E").fold(identity)(Left.apply) == Left("E")
        )

      }
    }

    "flatMap method called " should {
      "return correct results" in {
        assert(
          Right(1).flatMap(_ => Right("1")) == Right("1")
        )

        assert(
          Left("E").flatMap(identity) == Left("E")
        )

      }
    }

    "map method called " should {
      "return correct results" in {
        assert(
          Right(1).map(_ + 1) == Right(2)
        )

        assert(
          Left("E").map(identity) == Left("E")
        )

      }
    }

    "orElse method called " should {
      "return correct results" in {
        assert(
          Right(1).orElse(Left("A")) == Right(1)
        )

        assert(
          Left("E").orElse(Right(42)) == Right(42)
        )

      }
    }

    "map2 method called " should {
      "return correct results" in {
        assert(Right(1).map2(Right(0.5d))((a, b) => a + b) == Right(1.5d))
        assert(
          Left("A").map2(Right(0.5d))((_, _) => fail("should be Left")) == Left(
            "A"
          )
        )
        assert(
          Right(0.5d).map2(Left("A"))((_, _) => fail("should be Left")) == Left(
            "A"
          )
        )
      }
    }

  }

}
