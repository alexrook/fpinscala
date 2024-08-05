package fpinscala.laziness

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Assertions._
import org.scalatest.matchers.should.Matchers._
import fpinscala.parallelism.Par

class StreamTest extends AnyWordSpec {

  import fpinscala.laziness.Stream
  import fpinscala.laziness.Stream._

  "Custom Stream" when {

    "flatMap method called " should {
      "return correct results for non empty list" in {
        Stream(
          1, 2, 3, 4, 5
        ).flatMap(x => Stream(x)).toList shouldBe List(1, 2, 3, 4, 5)

        Stream("A").flatMap(s => Stream(s, s)).toList shouldBe List("A", "A")

        Stream(5, 4, 3, 2, 1).flatMap(x => Stream(x + 1)).toList shouldBe List(
          6, 5, 4, 3, 2)

        Stream("A", "B", "C")
          .flatMap(_ => Stream(1, 2, 3))
          .toList shouldBe List(1, 2, 3, 1, 2, 3, 1, 2, 3)
      }

      "return correct results for empty list" in {
        Stream.empty[Int].flatMap(_ => Stream(1, 2, 3)) shouldBe Stream
          .empty[Int]

        Stream.empty[Int].flatMap(_ => Stream.empty[Int]) shouldBe Stream
          .empty[Int]
      }
    }

    "appendOne method called " should {
      "return correct results for non empty list" in {
        Stream(
          1, 2, 3, 4, 5
        ).appendOne(6).toList shouldBe List(1, 2, 3, 4, 5, 6)

        Stream("A").appendOne(1).toList shouldBe List("A", 1)

        Stream(5, 4, 3, 2, 1).appendOne(1).toList shouldBe List(5, 4, 3, 2, 1,
          1)

        Stream("A", "B", "C").appendOne("D").toList shouldBe List(
          "A",
          "B",
          "C",
          "D"
        )
      }

      "return correct results for empty list" in {
        Stream.empty[Int].appendOne(42).toList shouldBe List(42)
      }
    }
    
    "append method called " should {
      "return correct results for non empty list" in {
        Stream(
          1, 2, 3, 4, 5
        ).append(Stream(6)).toList shouldBe List(1, 2, 3, 4, 5, 6)

        Stream("A").append(Stream(1, 3, 4)).toList shouldBe List("A", 1, 3, 4)

        Stream(5, 4, 3, 2, 1).append(Stream(1, 2, 3)).toList shouldBe List(5, 4,
          3, 2, 1, 1, 2, 3)

        Stream(5, 4, 3, 2, 1)
          .append(Stream.empty[Int])
          .toList shouldBe List(5, 4, 3, 2, 1)

        Stream("A", "B", "C").append(Stream.empty).toList shouldBe List(
          "A",
          "B",
          "C"
        )
      }

      "return correct results for empty list" in {
        Stream.empty[Int].filter(_ == 1) shouldBe Stream.empty[Int]
      }
    }

    "filter method called " should {
      "return correct results for non empty list" in {
        Stream(
          1, 2, 3, 4, 5
        ).filter(_ > 1).toList shouldBe List(2, 3, 4, 5)

        Stream("A").filter(_.nonEmpty).toList shouldBe List("A")

        Stream(5, 4, 3, 2, 1).filter(_ < 6).toList shouldBe List(5, 4, 3, 2, 1)

        Stream(5, 4, 3, 2, 1)
          .filter(x => x == 3 || x == 5)
          .toList shouldBe List(5, 3)

        Stream(5, 4, 3, 2, 1)
          .filter(x => x == 4 || x == 2)
          .toList shouldBe List(4, 2)

        Stream("A", "B", "C").filter(_ => false) shouldBe Stream.empty
      }

      "return correct results for empty list" in {
        Stream.empty[Int].filter(_ == 1) shouldBe Stream.empty[Int]
      }
    }

    "map method called " should {
      "return correct results for non empty list" in {
        Stream(
          1, 2, 3, 4, 5
        ).map(_ + 1).toList shouldBe List(2, 3, 4, 5, 6)

        Stream("A").map(_ => "B").toList shouldBe List("B")
        Stream(5, 4, 3, 2, 1).map(_ - 1).toList shouldBe List(4, 3, 2, 1, 0)
      }

      "return correct results for empty list" in {
        Stream.empty[Int].map(_ * 2) shouldBe Stream.empty[Int]
      }
    }

    "headOption method called " should {
      "return correct results for non empty list" in {
        Stream(1, 2, 3, 4, 5).headOption shouldBe Some(1)
        Stream("A").headOption shouldBe Some("A")
        Stream(5, 4, 3, 2, 1).headOption shouldBe Some(5)
      }

      "return correct results for empty list" in {
        Stream.empty[Int].headOption shouldBe None
      }
    }

    "takeWhile2 method called " should {
      "return correct results for non empty list" in {
        Stream(1, 2, 3, 4, 5).takeWhile2(_ > 0).toList shouldBe List(1, 2, 3, 4,
          5)
        Stream("A").takeWhile2(_.length() > 2) shouldBe Stream.empty[String]
        Stream(1, 2, 3, 4, 5).takeWhile2(_ == Int.MaxValue) shouldBe Stream
          .empty[Int]
        Stream(5, 4, 3, 2, 1).takeWhile2(_ > 2).toList shouldBe List(5, 4, 3)
      }

      "return correct results for empty list" in {
        Stream.empty[Int].takeWhile2(_ < 0) shouldBe Stream.empty[Int]
        Stream.empty[Int].takeWhile2(_ > Int.MaxValue) shouldBe Stream
          .empty[Int]
        Stream.empty[Int].takeWhile2(_ < Int.MinValue) shouldBe Stream
          .empty[Int]
      }

    }

    "forAll method called " should {
      "return correct results for non empty list" in {
        Stream(1, 2, 3, 4, 5).forAll(_ > 0) shouldBe true
        Stream("A").forAll(_.length() > 2) shouldBe false
        Stream(1, 2, 3, 4, 5).forAll(_ == Int.MaxValue) shouldBe false
        Stream(5, 4, 3, 2, 1).forAll(_ > 0) shouldBe true
      }

      "return correct results for empty list" in {
        Stream.empty[Int].forAll(_ < 0) shouldBe false
        Stream.empty[Int].forAll(_ > Int.MaxValue) shouldBe false
        Stream.empty[Int].forAll(_ < Int.MinValue) shouldBe false
      }
    }

    "takeWhile method called " should {
      "return correct results for non empty list" in {
        Stream(1, 2, 3, 4, 5).takeWhile(_ > 0).toList shouldBe List(1, 2, 3, 4,
          5)
        Stream("A").takeWhile(_.length() > 2) shouldBe Stream.empty[String]
        Stream(1, 2, 3, 4, 5).takeWhile(_ == Int.MaxValue) shouldBe Stream
          .empty[Int]
        Stream(5, 4, 3, 2, 1).takeWhile(_ > 2).toList shouldBe List(5, 4, 3)
      }

      "return correct results for empty list" in {
        Stream.empty[Int].takeWhile(_ < 0) shouldBe Stream.empty[Int]
        Stream.empty[Int].takeWhile(_ > Int.MaxValue) shouldBe Stream.empty[Int]
        Stream.empty[Int].takeWhile(_ < Int.MinValue) shouldBe Stream.empty[Int]
      }

    }

    "drop method called " should {
      "return correct results for non empty list" in {
        Stream(1, 2, 3, 4, 5).drop(2).toList shouldBe List(3, 4, 5)
        Stream("A").drop(1) shouldBe Stream.empty[String]
        Stream(1, 2, 3, 4, 5).drop(Int.MaxValue) shouldBe Stream.empty[Int]
        Stream(1, 2, 3, 4).drop(-1).toList shouldBe List(1, 2, 3, 4)
      }

      "return correct results for empty list" in {
        Stream.empty[Int].drop(10) shouldBe Stream.empty[Int]
        Stream.empty[Int].drop(Int.MaxValue) shouldBe Stream.empty[Int]
        Stream.empty[Int].drop(Int.MinValue) shouldBe Stream.empty[Int]
      }

    }

    "take method called " should {
      "return correct results for non empty list" in {
        Stream(1, 2, 3, 4, 5).take(2).toList shouldBe List(1, 2)
        Stream(1).take(1).toList shouldBe Stream(1).toList
        Stream(1, 2, 3, 4, 5)
          .take(Int.MaxValue)
          .toList shouldBe List(1, 2, 3, 4, 5)
        Stream(1, 3, 4).take(-1) shouldBe Stream.empty[Int]
      }

      "return correct results for empty list" in {
        Stream.empty[Int].take(10) shouldBe Stream.empty[Int]
        Stream.empty[Int].take(Int.MaxValue) shouldBe Stream.empty[Int]
        Stream.empty[Int].take(Int.MinValue) shouldBe Stream.empty[Int]
      }

    }

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
