package fpinscala.errorhandling

import scala.{
  Option => _,
  Some => _,
  None => _,
  Either => _,
  _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

import org.scalatest.wordspec.AnyWordSpec
import scala.concurrent.Future.never

class ListTest extends AnyWordSpec {

  import fpinscala.errorhandling.Option._
  import fpinscala.errorhandling.Option

  "Custom Option" when {

    "sequence method called" should {
      "return correct results" in {
        // copy from branch/second-edition/tests
        def expected(xs: List[Option[Int]]): Option[List[Int]] =
          if (xs.contains(None)) {
            None
          } else {
            Some(xs.flatMap(_.map(List(_)).getOrElse(List.empty[Int])))
          }

        val xs1 = List(Some(1), Some(2), Some(2))
        val expected1 = expected(xs1)
        val actual1 = sequence(xs1)
        assert(actual1 == expected1)

        val xs2 = List(Some(1), None, Some(2))
        val expected2 = expected(xs2)
        val actual2 = sequence(xs2)
        assert(actual2 == expected2)

      }
    }

    "map2 method called " should {
      "return correct results" in {

        assert(
          map2(Some(1), Some(2))(_ + _) == Some(3)
        )

        assert(
          map2(None: Option[Int], Some(2))(_ + _) == None
        )

        assert(
          map2(Some(2), None: Option[Int])(_ + _) == None
        )

        assert(
          map2(None: Option[Int], None: Option[Int])(_ + _) == None
        )

      }
    }

    "variance method called " should {
      "return correct results" in {

        def expected( // just copy from second-edition branch /tests
            xs: Seq[Double]
        ): Option[Double] =
          if (xs.nonEmpty) {
            val m = xs.sum / xs.length
            val newList = xs.map(x => math.pow(x - m, 2))
            Some(newList.sum / newList.length)
          } else {
            None
          }
        val xs1 = Seq(1, 2, 3, 4, 56d)
        val actual1 = variance(xs1)
        assert(actual1 == expected(xs1))

        val xs2 = Seq(0.01, -2.34, 3.5, 4, 56d)
        val actual2 = variance(xs2)
        assert(actual2 == expected(xs2))

      }
    }

    "flatten method called" should {
      "return None for an empty Option" in {
        assert(Some(None).flatten == None)

      }

      "return Some for non empty option" in {
        assert(Some(Some(1)).flatten == Some(1))
      }
    }

    "flatMap method called" should {
      "return None for an empty Option" in {
        assert(None.flatMap(_ => sys.error("never")) == None)

      }

      "return Some for non empty option" in {
        assert(Some(1).flatMap(Some.apply) == Some(1))
      }
    }

    "orElse method called" should {
      "return Some for an empty Option" in {
        assert(None.orElse(Some(1)) == Some(1))
      }

      "return None for an empty Option" in {
        assert(None.orElse(None) == None)
      }

      "return the same Some for non empty option" in {
        assert(Some(1).orElse(Some(2)) == Some(1))
      }
    }

    "filter method called" should {
      "return None for an empty Option" in {
        assert(None.filter(_ => sys.error("never")) == None)
      }

      "return the Some for non empty option" in {
        assert(Some(1).filter(x => x > 0) == Some(1))
      }

      "return the None when filters out non empty option" in {
        assert(Some(1).filter(x => x < 0) == None)
      }
    }

  }

}
