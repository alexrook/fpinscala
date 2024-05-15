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

}
