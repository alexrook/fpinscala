package fpinscala.laziness

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Assertions._
import org.scalatest.matchers.should.Matchers._
import fpinscala.parallelism.Par

class StreamTest extends AnyWordSpec {

  import fpinscala.laziness.Stream
  import fpinscala.laziness.Stream._

  "Custom Stream" when {

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
