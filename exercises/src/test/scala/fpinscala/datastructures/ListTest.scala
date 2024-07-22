package fpinscala.datastructures

import fpinscala.iomonad.IO1.IO
import org.scalatest.wordspec.AnyWordSpec

class ListTest extends AnyWordSpec {

  import fpinscala.datastructures.List._
  import fpinscala.datastructures.{List => CL, Nil => CNil}

  "Custom List" when {
    "tail method called" should {
      "return tail for non empty lists" in {
        assert(
          tail(CL(1, 2, 3, 4)) match {
            case Cons(_, _) => true
            case Nil        => false
          }
        )
        assert(
          tail(CL(1)) match {
            case Cons(_, _) => false
            case Nil        => true
          }
        )
      }

      "throw error for empty list" in {
        assertThrows[NoSuchElementException](
          tail(Nil) match {
            case Cons(_, _) => true
            case Nil        => false
          }
        )
      }
    }

  }

  "drop method called" should {
    "retun the same List if n==0" in {
      val l = CL(1, 2, 3)
      assert(drop(l, 0) == l)
    }

    "retun Nil for empty Lists" in {
      assert(drop(Nil, 0) == Nil)
    }

    "retun right results for non empty Lists" in {
      assert(drop(CL(1, 2, 3), 2) == CL(3))
      assert(drop(CL(1, 2, 3, 4, 5), 2) == CL(3, 4, 5))
    }
  }

  "init method called" should {
    "throw an error for empty List" in {
      assertThrows[NoSuchElementException](init(Nil))
    }

    "return Nil for one elem List" in {
      assert(init(CL(1)) == Nil)
    }

    "return List with first elem if arg has two elems" in {
      assert(init(CL(1, 2)) == CL(1))
    }

    "return correct results for non empty Lists" in {
      assert(init(CL(1, 2, 3)) == CL(1, 2))
      assert(init(CL(7, 2, 3, 5)) == CL(7, 2, 3))
    }

  }

  "foldRight method" should {
    "run from right to left" in {
      assert(
        CL
          .foldRight(CL(1, 2, 3), "") { case (e, acc) =>
            s"$acc $e"
          }
          .trim == "3 2 1"
      )
    }
  }

  "foldRight2 method" should {
    "run from right to left" in {
      assert(
        CL
          .foldRight2(CL(1, 2, 3), "") { case (e, acc) =>
            s"$acc $e"
          }
          .trim == "3 2 1"
      )
    }
  }

  "foldLeft method" should {
    "run from left to right" in {
      assert(
        CL
          .foldLeft(CL(1, 2, 3), "") { case (acc, elem) =>
            s"$acc $elem"
          }
          .trim == "1 2 3"
      )
    }
  }

  "reverse method" should {
    "return Nil for empty List" in {
      assert(reverse(Nil) == Nil)
    }

    "return correct results for non empty List's" in {
      assert(reverse(CL(1)) == CL(1))
      assert(reverse(CL(1, 2)) == CL(2, 1))
      assert(reverse(CL(1, 2, 3)) == CL(3, 2, 1))
    }

  }

  "append method" should {
    "return two list concatenation" in {
      assert(append(CL(1, 2, 3), CL(4, 5, 6)) == CL(1, 2, 3, 4, 5, 6))
    }
  }

  "appendViaFoldLeft method" should {
    "return two list concatenation" in {
      assert(
        appendViaFoldLeft(CL(1, 2, 3), CL(4, 5, 6)) == CL(1, 2, 3, 4, 5, 6)
      )
    }
  }

  "appendViaFoldRight method" should {
    "return two list concatenation" in {
      assert(
        appendViaFoldRight(CL(1, 2, 3), CL(4, 5, 6)) == CL(1, 2, 3, 4, 5, 6)
      )
    }
  }

  "the map method" should {
    "return Nil for an empty List" in {
      assert(map(Nil)(identity) == Nil)
    }

    "return correct result for one elem List" in {
      assert(map(CL(1))(_ + 1) == CL(2))
    }

    "return correct result for non empty List's" in {
      assert(map(CL(1, 3))(_ + 1) == CL(2, 4))
      assert(map(CL(5, 1, 3))(_ + 1) == CL(6, 2, 4))
    }
  }

  "the filter method" should {
    "return Nil for an empty List" in {
      assert(filter(Nil)(identity) == Nil)
    }

    "return correct result for one elem List" in {
      assert(filter(CL(1))(_ == 1) == CL(1))
    }

    "return correct result for non empty List's" in {
      assert(filter(CL(1, 3))(_ > 1) == CL(3))
      assert(filter(CL(5, 1, 3))(_ > 1) == CL(5, 3))
    }
  }

  "the flatMap method" should {
    "return Nil for an empty List" in {
      assert(flatMap(Nil)(CL(_)) == Nil)
    }

    "return correct result for one elem List" in {
      assert(flatMap(CL(1))(x => CL(x, x)) == CL(1, 1))
    }

    "return correct result for non empty List's" in {
      assert(flatMap(List(1, 2, 3))(x => List(x, x)) == CL(1, 1, 2, 2, 3, 3))
      assert(flatMap(CL(5, 1, 3))(x => List(x, x)) == CL(5, 5, 1, 1, 3, 3))
    }
  }

  "the addCorresponding method" should {
    "return Nil for an empty List" in {
      assert(addCorresponding(Nil, CL(1)) == Nil)
    }

    "return result for one elem List's" in {
      assert(addCorresponding(CL(1), CL(2)) == CL(3))
    }

    "return correct result for non empty List's" in {
      assert(addCorresponding(CL(1, 2, 3), CL(4, 5, 6)) == CL(5, 7, 9))
      assert(addCorresponding(CL(1, 2, 3, 5), CL(4, 5, 6)) == CL(5, 7, 9, 5))
      assert(addCorresponding(CL(1, 2, 3), CL(4, 5)) == CL(5, 7, 3))
    }
  }

  "the hasSubsequence method" should {
    "return true for an empty List" in {
      assert(hasSubsequenceV1(CNil, CNil))
    }

    "return true for the one size List" in {
      assert(hasSubsequenceV1(sup = CL(1), sub = CL(1, 2)))
      assert(hasSubsequenceV1(sup = CL(1), sub = CL(2, 1, 3)))
      assert(hasSubsequenceV1(sup = CL(3), sub = CL(2, 1, 3)))
    }

    "return true for task description data" in {
      val cl = CL(1, 2, 3, 4)
      // List(1,2), List(2,3), and List(4) as subsequences, among others.
      assert(hasSubsequenceV1(sup = CL(1, 2), sub = cl))
      assert(hasSubsequenceV1(sup = CL(2, 3), sub = cl))
      assert(hasSubsequenceV1(sup = CL(4), sub = cl))
      assert(hasSubsequenceV1(sup = CNil, sub = cl))
      assert(hasSubsequenceV1(sup = CL(3, 4), sub = cl))
    }

  }

}
