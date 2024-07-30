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
