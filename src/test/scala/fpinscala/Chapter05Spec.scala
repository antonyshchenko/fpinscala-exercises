package fpinscala.Chapter05

import org.scalatest._

class Chapter05Spec extends FunSpec with Matchers {
  describe("toList") {
    it("converts Stram to List") {
      Stream(1, 2, 3).toList shouldEqual List(1, 2, 3)
    }

    it("returns empty list when called on empty stream") {
      Stream.empty[Int].toList shouldEqual List()
    }
  }

  describe("take") {
    it("returns first n elements of the stream") {
      Stream(1, 2, 3, 4, 5).take(2).toList shouldEqual List(1, 2)
    }

    it("returns empty stream when n == 0") {
      Stream(1, 2, 3).take(0).toList shouldEqual List()
    }

    it("returns empty stream when n < 0") {
      Stream(1, 2, 3).take(-1).toList shouldEqual List()
    }

    it("returns whole stream when n > than length of the stream") {
      Stream(1, 2, 3).take(5).toList shouldEqual List(1, 2, 3)
    }
  }

  describe("drop") {
    it("returns a stream without first n elements") {
      Stream(1, 2, 3, 4, 5).drop(2).toList shouldEqual List(3, 4, 5)
    }

    it("returns the whole stream when n == 0") {
      Stream(1, 2, 3).drop(0).toList shouldEqual List(1, 2, 3)
    }

    it("returns empty list when n == list length") {
      Stream(1, 2, 3).drop(3).toList shouldEqual List()
    }

    it("returns empty list when n > list length") {
      Stream(1, 2, 3).drop(4).toList shouldEqual List()
    }
  }

  describe("takeWhile") {
    val p = (x: Int) => x % 2 == 0

    it("returns all starting elements of a stream that match a given predicate") {
      Stream(2, 4, 5, 6).takeWhile(p).toList shouldEqual List(2, 4)
    }

    it("returns the whole stream when all elements match given predicate") {
      Stream(2, 4, 6).takeWhile(p).toList shouldEqual List(2, 4, 6)
    }

    it("returns empty stream when first element does not match given predicate") {
      Stream(1, 2, 4).takeWhile(p).toList shouldEqual List()
    }

    it("returns empty stream when called on empty stream") {
      Stream.empty.takeWhile(p).toList shouldEqual List()
    }
  }

  describe("forAll") {
    val p = (x: Int) => x % 2 == 0

    it("returns true when all elements of the stream match given predicate") {
      Stream(2, 4, 6).forAll(p) shouldEqual true
    }

    it("returns false when stram contains at least one element that does not match given predicate") {
      Stream(2, 4, 5).forAll(p) shouldEqual false
    }

    it("returns true when stream is empty") {
      Stream.empty.forAll(p) shouldEqual true
    }
  }

  describe("takeWhileUsingFoldRight") {
    val p = (x: Int) => x % 2 == 0

    it("returns all starting elements of a stream that match a given predicate") {
      Stream(2, 4, 5, 6).takeWhileUsingFoldRight(p).toList shouldEqual List(2, 4)
    }

    it("returns the whole stream when all elements match given predicate") {
      Stream(2, 4, 6).takeWhileUsingFoldRight(p).toList shouldEqual List(2, 4, 6)
    }

    it("returns empty stream when first element does not match given predicate") {
      Stream(1, 2, 4).takeWhileUsingFoldRight(p).toList shouldEqual List()
    }

    it("returns empty stream when called on empty stream") {
      Stream.empty.takeWhileUsingFoldRight(p).toList shouldEqual List()
    }
  }

  describe("headOption") {
    it("returns Some with head when stream is not empty") {
      Stream(1, 2, 3).headOption shouldEqual Some(1)
    }

    it("returns None when stream is empty") {
      Stream.empty[Int].headOption shouldEqual None
    }
  }
}
