package fpinscala

import org.scalatest._

class Chapter03Spec extends FunSpec with Matchers {
  describe("List") {
    describe("tail") {
      it("returns a list without first element") {
        List.tail(List(1, 2, 3)) shouldEqual List(2, 3)
      }

      it("throws exception when called on empty list") {
        an [RuntimeException] should be thrownBy {
          List.tail(List())
        }
      }
    }

    describe("setHead") {
      it("returns list with changed head") {
        List.setHead(List(1, 2, 3), 4) shouldEqual List(4, 2, 3)
      }

      it("throws exception when called on empty list") {
        an [RuntimeException] should be thrownBy {
          List.setHead(List(), 1)
        }
      }
    }

    describe("drop") {
      it("returns a list without first N elements") {
        List.drop(List(1, 2, 3), 2) shouldEqual List(3)
      }

      it("returns empty list when called on empty list") {
        List.drop(List(), 2) shouldEqual List()
      }

      it("returns same list when N is 0") {
        List.drop(List(1, 2, 3), 0) shouldEqual List(1, 2, 3)
      }

      it("returns same list when N is < 0") {
        List.drop(List(1, 2, 3), -1) shouldEqual List(1, 2, 3)
      }

      it("returns empty list when N is greater than the length of the original list") {
        List.drop(List(1, 2, 3), 5) shouldEqual List()
      }
    }

    describe("dropWhile") {
      val predicate = (x: Int) => x < 4

      it("returns a list without first elements that match predicate") {
        List.dropWhile(List(1, 2, 3, 4, 5), predicate) shouldEqual List(4, 5)
      }

      it("returns empty list when called on empty list") {
        List.dropWhile(List(), predicate) shouldEqual List()
      }

      it("returns same list when there are no elements that match predicate") {
        List.dropWhile(List(4, 5, 6), predicate) shouldEqual List(4, 5, 6)
      }
    }

    describe("init") {
      it("returns a list without last element") {
        List.init(List(1, 2, 3)) shouldEqual List(1, 2)
      }

      it("returns empty list when called on list with one element") {
        List.init(List(1)) shouldEqual List()
      }

      it("throws exception when called on empty list") {
        an [RuntimeException] should be thrownBy {
          List.init(List())
        }
      }
    }

    describe("length") {
      it("returns length of the list") {
        List.length(List(1, 2, 3)) shouldEqual 3
      }

      it("returns 0 when called on empty list") {
        List.length(List()) shouldEqual 0
      }
    }

    describe("foldLeft") {
      it("folds list to a single value") {
        List.foldLeft(List(1, 2, 3), 1)((x, y) => x + y) shouldEqual 7
      }

      it("returns seed when called on an empty list") {
        List.foldLeft(List[Int](), 1)((x, y) => x + y) shouldEqual 1
      }
    }

    describe("map") {
      it("returns list consisting of elements transformed with given function") {
        List.map(List(1, 2, 3))((x) => - x) shouldEqual List(-1, -2, -3)
      }

      it("returns empty list when called on empty list") {
        List.map(List[Int]())((x) => x) shouldEqual List[Int]()
      }
    }
  }
}

