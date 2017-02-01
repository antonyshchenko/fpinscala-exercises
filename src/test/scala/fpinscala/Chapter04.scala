package fpinscala.Chapter04

import org.scalatest._
import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

class Chapter04Spec extends FunSpec with Matchers {
  describe("map") {
    it("returns None when called on None") {
      val option: Option[Int] = None
      option.map(x => x * 2) shouldEqual None
    }

    it("returns Some with transformed value when called on Some") {
      Some(2).map(x => x * 2) shouldEqual Some(4)
    }
  }

  describe("getOrElse") {
    it("returns value of Some") {
      Some(2).getOrElse(123) shouldEqual 2
    }

    it("returns default value when called on None") {
      val option: Option[Int] = None
      option.getOrElse(123) shouldEqual 123
    }
  }

  describe("flatMap") {
    it("returns None when called on None") {
      val option: Option[Int] = None
      option.flatMap(x => Some(x)) shouldEqual None
    }

    it("returns an Option that is a result of a map operation on a value") {
      Some(2).flatMap(x => Some(x * 2)) shouldEqual Some(4)
    }

    it("returns None when mapping function returns None") {
      Some(2).flatMap(x => None) shouldEqual None
    }
  }

  describe("orElse") {
    it("returns an Option when called on Some") {
      Some(2).orElse(Some(3)) shouldEqual Some(2)
    }

    it("returns alternative Option when called on None") {
      val option: Option[Int] = None
      option.orElse(Some(3)) shouldEqual Some(3)
    }
  }

  describe("filter") {
    it("returns None when called on None") {
      val option: Option[Int] = None
      option.filter((x) => x % 2 == 0) shouldEqual None
    }

    it("returns Some when called on Some and value conforms the given predicate") {
      Some(2).filter((x) => x % 2 == 0) shouldEqual Some(2)
    }

    it("returns None when called on Some but value does not conform the given predicate") {
      Some(3).filter((x) => x % 2 == 0) shouldEqual None
    }
  }

  describe("variance") {
    it("returns None when called on empty sequence") {
      Option.variance(List[Double]()) shouldEqual None
    }

    it("returns Some with variance calc result when called on non-empty sequence") {
      Option.variance(List(1, 2, 3, 4, 5)) shouldEqual Some(2.0)
    }
  }

  describe("map2") {
    val f = (x: Int, y: Int) => x + y

    it("returns None when one or both of the arguments is None") {
      Option.map2(None, None)(f) shouldEqual None
      Option.map2(Some(1), None)(f) shouldEqual None
      Option.map2(None, Some(2))(f) shouldEqual None
    }

    it("returns Some with result of applying given function to both arguments") {
      Option.map2(Some(1), Some(2))(f) shouldEqual Some(3)
    }
  }

  describe("sequence") {
    it("returns Some with empty list when called on empty list") {
      Option.sequence(List[Option[Int]]()) shouldEqual Some(List())
    }

    it("returns Some with list of values when given list of options does not contain None") {
      Option.sequence(List(Some(1), Some(2))) shouldEqual Some(List(1, 2))
    }

    it("returns None when given list of options contains None") {
      Option.sequence(List(Some(1), None)) shouldEqual None
    }
  }

  describe("traverse") {
    val f = (x: Int) => if (x < 0) None else Some(Math.sqrt(x))

    it("returns Some with empty list when called on empty list") {
      Option.traverse(List[Int]())(f) shouldEqual Some(List())
    }

    it("returns Some with list of results of applying given function to elements of the list") {
      Option.traverse(List(4, 16))(f) shouldEqual Some(List(2, 4))
    }

    it("returns None when results of applying given function to elements of the list contains None") {
      Option.traverse(List(4, -4))(f) shouldEqual None
    }
  }
}

