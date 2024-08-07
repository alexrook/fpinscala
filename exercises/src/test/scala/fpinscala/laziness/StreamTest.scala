package fpinscala.laziness

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Assertions._
import org.scalatest.matchers.should.Matchers._
import fpinscala.parallelism.Par

class StreamTest extends AnyWordSpec {

  import fpinscala.laziness.Stream
  import fpinscala.laziness.Stream._

  "Custom Stream" when {

    "scanRight method called " should {

      "return correct results for non empty streams" in {
        // from exercise
        Stream(1, 2, 3).scanRightV1(0)(_ + _).toList shouldBe List(
          1 + 2 + 3 + 0,
          2 + 3 + 0,
          3 + 0,
          0
        )

        Stream(1, 2, 3, 4, 5).scanRightV1(0)(_ + _).toList shouldBe List(
          5 + 4 + 3 + 2 + 1,
          5 + 4 + 3 + 2,
          5 + 4 + 3,
          5 + 4,
          5 + 0,
          0
        )

        val stream = Stream(1, 2, 3, 4, 5)
        stream.scanRightV1(1)(_ * _).toList shouldBe
          stream.tails.map(_.toList.product).toList

      }

      // "infinite stream called" in {
      //   val infiniteStream: Stream[Int] = constant2(1)

      //   infiniteStream
      //     .scanRightV1(0)(_ + _)
      //     .take(5)
      //     .toList shouldBe List(5, 4, 3, 2, 1)

      //   infiniteStream
      //     .scanRight(0)(_ + _)
      //     .take(5)
      //     .toList shouldBe List(5, 4, 3, 2, 1)

      // }

      "return correct results for empty streams" in {
        Stream.empty[Int].scanRightV1(0)(_ + _).toList shouldBe List(
          0
        ) // TODO or List.empty ?
      }

    }

    "tails method called " should {

      "return correct results for non empty streams" in {
        Stream(1, 2, 3).tails.toList.map(_.toList) shouldBe List(
          List(1, 2, 3),
          List(2, 3),
          List(3),
          List.empty[Int]
        )

        Stream(1, 2, 3, 4, 5).tails.toList.map(_.toList) shouldBe List(
          List(1, 2, 3, 4, 5),
          List(2, 3, 4, 5),
          List(3, 4, 5),
          List(4, 5),
          List(5),
          List.empty[Int]
        )
      }

      "return correct results for empty streams" in {
        Stream.empty[Int].tails.toList.map(_.toList) shouldBe List(
          List.empty[Int]
        )
      }

    }

    "startsWith2 method called " should {
      "return correct results for non empty streams" in {

        Stream(1, 2, 3, 4, 5)
          .startsWith2(Stream(1, 2, 3, 4, 5)) shouldBe true

        Stream(1, 2, 3, 4, 5)
          .startsWith2(Stream(1, 2, 3)) shouldBe true

        Stream(1, 2, 3)
          .startsWith2(Stream(1, 2, 3, 4, 5)) shouldBe false

      }

      "return correct results for empty streams" in {
        // TODO: see https://github.com/fpinscala/fpinscala/discussions/696

        // Stream(1, 2, 3)
        //   .startsWith2(Stream.empty[Int]) shouldBe true

        // Stream
        //   .empty[Int]
        //   .startsWith2(Stream.empty[Int]) shouldBe true

        Stream
          .empty[Int]
          .startsWith2(Stream(1, 2, 3)) shouldBe false

      }

    }

    "startsWith method called " should {
      "return correct results for non empty streams" in {

        Stream(1, 2, 3, 4, 5)
          .startsWith(Stream(1, 2, 3, 4, 5)) shouldBe true

        Stream(1, 2, 3, 4, 5)
          .startsWith(Stream(1, 2, 3)) shouldBe true

        Stream(1, 2, 3)
          .startsWith(Stream(1, 2, 3, 4, 5)) shouldBe false

      }

      "return correct results for empty streams" in {
        Stream(1, 2, 3)
          .startsWith(Stream.empty[Int]) shouldBe true

        Stream
          .empty[Int]
          .startsWith(Stream.empty[Int]) shouldBe true

        Stream
          .empty[Int]
          .startsWith(Stream(1, 2, 3)) shouldBe false

      }

    }

    "zipAll method called " should {

      def excepted[A, B](
          left: List[A],
          right: List[B]
      ): List[(Option[A], Option[B])] =
        left.map(Some.apply).zipAll(right.map(Some.apply), None, None)
      "return correct results for non empty streams" in {
        Stream(1, 2, 3, 4, 5)
          .zipAll(Stream(1, 2, 3, 4, 5))
          .toList shouldBe
          excepted(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5))

        Stream(1, 2, 3, 4, 5)
          .zipAll(Stream(1, 2, 3))
          .toList shouldBe
          excepted(List(1, 2, 3, 4, 5), List(1, 2, 3))

        Stream(1, 2, 3)
          .zipAll(Stream(1, 2, 3, 4, 5))
          .toList shouldBe
          excepted(List(1, 2, 3), List(1, 2, 3, 4, 5))
      }

      "return correct results for empty streams" in {
        Stream
          .empty[Int]
          .zipAll(Stream(1, 2, 3, 4, 5))
          .toList shouldBe excepted(
          List.empty[Int],
          List(1, 2, 3, 4, 5)
        )

        Stream(1, 2, 3)
          .zipAll(Stream.empty[Int])
          .toList shouldBe excepted(
          List(1, 2, 3),
          List.empty[Int]
        )

      }

    }

    "zipWith method called " should {
      "return correct results for non empty stream" in {
        Stream(1, 2, 3, 4, 5)
          .zipWith(Stream(1, 2, 3, 4, 5))(_ + _)
          .toList shouldBe
          List(2, 4, 6, 8, 10)

        Stream(1, 2, 3, 4, 5)
          .zipWith(Stream(1, 2, 3))(_ + _)
          .toList shouldBe
          List(2, 4, 6)

        Stream(1, 2, 3)
          .zipWith(Stream(1, 2, 3, 4, 5))(_ + _)
          .toList shouldBe
          List(2, 4, 6)
      }

      "return correct results for empty stream" in {
        Stream
          .empty[Int]
          .zipWith(Stream(1, 2, 3, 4, 5))(_ + _) shouldBe Stream.empty[Int]

        Stream(1, 2, 3)
          .zipWith(Stream.empty[Int])(_ + _) shouldBe Stream.empty[Int]

      }

    }

    "takeWhile3 method called " should {
      "return correct results for non empty list" in {
        Stream(1, 2, 3, 4, 5).takeWhile3(_ > 0).toList shouldBe List(1, 2, 3, 4,
          5)
        Stream("A").takeWhile3(_.length() > 2) shouldBe Stream.empty[String]
        Stream(1, 2, 3, 4, 5).takeWhile3(_ == Int.MaxValue) shouldBe Stream
          .empty[Int]
        Stream(5, 4, 3, 2, 1).takeWhile3(_ > 2).toList shouldBe List(5, 4, 3)
      }

      "return correct results for empty list" in {
        Stream.empty[Int].takeWhile3(_ < 0) shouldBe Stream.empty[Int]
        Stream.empty[Int].takeWhile3(_ > Int.MaxValue) shouldBe Stream
          .empty[Int]
        Stream.empty[Int].takeWhile3(_ < Int.MinValue) shouldBe Stream
          .empty[Int]
      }

    }

    "take2 method called " should {
      "return correct results for non empty list" in {
        Stream(1, 2, 3, 4, 5).take2(2).toList shouldBe List(1, 2)
        Stream(1).take2(1).toList shouldBe Stream(1).toList
        Stream(1, 2, 3, 4, 5)
          .take2(Int.MaxValue)
          .toList shouldBe List(1, 2, 3, 4, 5)
        Stream(1, 3, 4).take2(-1) shouldBe Stream.empty[Int]
      }

      "return correct results for empty list" in {
        Stream.empty[Int].take2(10) shouldBe Stream.empty[Int]
        Stream.empty[Int].take2(Int.MaxValue) shouldBe Stream.empty[Int]
        Stream.empty[Int].take2(Int.MinValue) shouldBe Stream.empty[Int]
      }

    }

    "map2 method called " should {
      "return correct results for non empty list" in {
        Stream(
          1, 2, 3, 4, 5
        ).map2(_ + 1).toList shouldBe List(2, 3, 4, 5, 6)

        Stream("A").map2(_ => "B").toList shouldBe List("B")
        Stream(5, 4, 3, 2, 1).map2(_ - 1).toList shouldBe List(4, 3, 2, 1, 0)
      }

      "return correct results for empty list" in {
        Stream.empty[Int].map2(_ * 2) shouldBe Stream.empty[Int]
      }
    }

    "constant2 method called" should {
      "return correct values" in {
        constant2("A").take(3).toList shouldBe List.fill(3)("A")
        constant2(1).take(10).toList shouldBe List.fill(10)(1)

        ones3.take(0) shouldBe Stream.empty[Int]
        ones3.take(42).toList shouldBe List.fill(42)(1)
      }
    }

    "from2 method called" should {
      "return correct values" in {
        from2(-1).take(5).toList shouldBe List(-1, 0, 1, 2, 3)

        from2(1).take(42).toList.contains(42) shouldBe true
      }
    }

    "fibs2 method called" should {
      "return correct values" in {
        fibs2.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
      }
    }

    "unfold method called" should {
      "return correct values" in {

        unfold(1) { x =>
          Some(x -> (x + 1))
        }.take(3).toList shouldBe List(1, 2, 3)

        unfold(1)(_ => None) shouldBe Stream.empty[Int]

        unfold(1) {

          case x if x < 5 =>
            Some(x -> (x + 1))

          case _ => None

        }.take(Int.MaxValue).toList shouldBe List(1, 2, 3, 4)
      }
    }

    "fibs method called" should {
      "return correct values" in {
        fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
      }
    }

    "from method called" should {
      "return correct values" in {
        from(1).take(3).toList shouldBe List(1, 2, 3)

        from(1).take(42).toList.contains(42) shouldBe true
      }
    }

    "constant method called" should {
      "return correct values" in {
        constant("A").take(3).toList shouldBe List.fill(3)("A")
        constant(1).take(10).toList shouldBe List.fill(10)(1)

        ones.take(0) shouldBe Stream.empty[Int]
        ones.take(42).toList shouldBe List.fill(42)(1)
      }
    }

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

        Stream((5, 4), (4, 4), (3, 3), (2, 2), (1, 1)).forAll { case ((a, b)) =>
          a == b
        } shouldBe false
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
