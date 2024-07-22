package fpinscala.datastructures
import fpinscala.iomonad.IO1.IO
import org.scalatest.wordspec.AnyWordSpec

class TreeTest extends AnyWordSpec {

  import fpinscala.datastructures.Tree._
  import fpinscala.datastructures.{Tree => CT, _}

  "Custom Tree" when {
    "size method called" should {
      "3 for two leaf tree" in {
        assert(
          size(Branch(Leaf(1), Leaf(1))) == 3
        )

      }

      "5 for one leaf and one branch tree" in {
        assert(
          size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5
        )

      }

    }

    "maximum method called" should {
      "max value for two leaf tree" in {
        assert(
          maximum(Branch(Leaf(1), Leaf(1))) == 1
        )

        assert(
          maximum(Branch(Leaf(1), Leaf(2))) == 2
        )
      }

      "max value for two branch tree" in {
        assert(
          maximum(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))) == 1
        )

        assert(
          maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3
        )
      }

    }

    "depth method called" should {
      "1 value for two leaf tree" in {
        assert(
          depth(Branch(Leaf(1), Leaf(1))) == 2
        )
      }

      "correct value for two branch tree" in {
        val d: Int =
          depth(
            Branch(
              left = Leaf(1),
              right = Branch(
                left = Branch(
                  left = Leaf(3),
                  right = Leaf(3)
                ),
                right = Leaf(2)
              )
            )
          )
        assert(
          d == 4
        )
      }
    }

    "map method called" should {
      "correct value for two leaf tree" in {
        assert(
          map(Branch(Leaf(1), Leaf(1)))(_.toString()) ==
            Branch(Leaf("1"), Leaf("1"))
        )
      }

      "correct value for two branch tree" in {
        assert(
          map(
            Branch(
              left = Leaf(1),
              right = Branch(
                left = Branch(
                  left = Leaf(3),
                  right = Leaf(3)
                ),
                right = Leaf(2)
              )
            )
          )(_.toString()) ==
            Branch(
              left = Leaf("1"),
              right = Branch(
                left = Branch(
                  left = Leaf("3"),
                  right = Leaf("3")
                ),
                right = Leaf("2")
              )
            )
        )
      }
    }

    "size2 method called" should {
      "3 for two leaf tree" in {
        assert(
          size2(Branch(Leaf(1), Leaf(1))) == 3
        )

      }

      "5 for one leaf and one branch tree" in {
        assert(
          size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5
        )

      }
    }

  }

}
