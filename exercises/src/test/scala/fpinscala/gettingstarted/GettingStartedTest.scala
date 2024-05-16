package fpinscala.gettingstarted

import org.scalatest.wordspec.AnyWordSpec

class GettingStartedTest extends AnyWordSpec {

  "MyModule.fib" when {
    "receives zero as arg" should {
      "return 0" in {
        assert(MyModule.fib(0) == 0)
      }
    }

    "receives 1 as arg" should {
      "return 1" in {
        assert(MyModule.fib(1) == 1)
      }
    }

    "receives 2 as arg" should {
      "return 1" in {
        assert(MyModule.fib(2) == 1)
      }
    }

    "receives 3 as arg" should {
      "return 2" in {
        assert(MyModule.fib(3) == 2)
      }
    }

    "receives 4 as arg" should {
      "return 3" in {
        assert(MyModule.fib(4) == 3)
      }
    }

    "receives 5 as arg" should {
      "return 5" in {
        assert(MyModule.fib(5) == 5)
      }
    }

  }

  "PolymorphicFunctions.isSorted" when {

    def gt(l: Int, r: Int): Boolean =
      if (l <= r) {
        true
      } else {
        false
      }

    "receives an empty array" should {
      "return true" in {
        assert(PolymorphicFunctions.isSorted(Array.empty[Int], gt))
      }
    }

    "gets an array of one element" should {
      "return true" in {
        assert(PolymorphicFunctions.isSorted(Array(1), gt))
      }
    }

    "gets a sorted array" should {
      "return true" in {
        assert(PolymorphicFunctions.isSorted(Array(1, 2, 3), gt))
        assert(PolymorphicFunctions.isSorted(Array(1, 3, 3, 4), gt))
      }
    }

    "gets an unsorted array" should {
      "return false" in {
        assertResult(false)(PolymorphicFunctions.isSorted(Array(11, 1, 3, 3, 4), gt))
        assertResult(false)(PolymorphicFunctions.isSorted(Array(0, 1, 3, 3, -11, 4), gt))
        assertResult(false)(PolymorphicFunctions.isSorted(Array(1, 0), gt))
      }
    }

  }

}
