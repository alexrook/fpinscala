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
import org.scalatest.matchers.should.Matchers._
import fpinscala.parallelism.Par

class EitherTest extends AnyWordSpec {

  import fpinscala.errorhandling.Either._
  import fpinscala.errorhandling.Either

  "Custom Either" when {

    "sequence method called " should {
      "return results for Right list" in {

        sequence(
          List(Right(1), Right(2), Right(3))
        ) shouldBe Right(List(1, 2, 3))

      }

      "return results for empty list" in {
        sequence(
          List.empty[Right[Int]]
        ) shouldBe Right(List.empty[Int])
      }

      "return results for Left list" in {

        sequence(
          List(Left("A"), Right(2), Right(3))
        ) shouldBe Left("A")

        sequence(
          List(Left("A"), Right(2), Left("B"))
        ) shouldBe Left("A")
      }
    }

    "traverse method called " should {
      "return results for Right list" in {
        assert(
          traverse(
            List(1, 2, 3)
          )(x => Right(x.toString())) ==
            Right(List("1", "2", "3"))
        )

      }

      "return results for empty list" in {
        assert(
          traverse(
            List.empty[Int]
          )(x => Right(x.toString())) ==
            Right(List.empty[Int])
        )

      }

      "return results for Left list" in {
        assert(
          traverse(
            List(1, 2, 3)
          )(x =>
            if (x == 1) {
              Left("A")
            } else {
              Right(x.toString())
            }
          ) ==
            Left("A")
        )
        // should return first error
        traverse(
          List(1, 2, 3)
        ) {
          case 1 => Left("A")
          case 2 => Left("B")
          case x => Right(x.toString())
        } shouldBe Left("A")

      }

    }

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
