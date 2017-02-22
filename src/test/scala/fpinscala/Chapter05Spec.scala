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

  describe("map") {
    val f = (x: Int) => x * 2

    it("transforms values in Stream with given function") {
      Stream(1, 2, 3).map(f).toList shouldEqual List(2, 4, 6)
    }

    it("returns empty stream when called on empty stream") {
      Empty.map(f).toList shouldEqual List()
    }
  }

  describe("filter") {
    val p = (x: Int) => x % 2 == 0

    it("returns stream of elements that conform to given predicate") {
      Stream(1, 2, 3, 4).filter(p).toList shouldEqual List(2, 4)
    }

    it("returns empty stream when called on empty stream") {
      Empty.filter(p).toList shouldEqual List()
    }
  }

  describe("append") {
    it("appends a stream to the end of current stream") {
      Stream(1, 2).append(Stream(3, 4)).toList shouldEqual List(1, 2, 3, 4)
    }

    it("returns original stream when appending empty stream") {
      Stream(1, 2, 3).append(Empty).toList shouldEqual List(1, 2, 3)
    }

    it("returns empty stream when appending empty stream to other empty stream") {
      Empty.append(Empty).toList shouldEqual List()
    }
  }

  describe("flatmap") {
    it("returns a flattened stream of elements as a result of mapping over given stream with a function that returns a stream for each element") {
      Stream(1, 2, 3).flatmap(i => Stream(i, i)).toList shouldEqual List(1, 1, 2, 2, 3, 3)
    }

    it("returns an empty stream when called on empty stream") {
      Empty.flatmap(i => Stream(i, i)).toList shouldEqual List()
    }
  }

  describe("Stream.constant") {
    it("returns infinite stream of given constant") {
      Stream.constant(2).take(3).toList shouldEqual List(2, 2, 2)
    }
  }

  describe("Stream.from") {
    it("returns infinite stream of integers starting from given n") {
      Stream.from(2).take(3).toList shouldEqual List(2, 3, 4)
    }
  }

  describe("Stream.fibs") {
    it("returns infinite stream of Fibonacci numbers") {
      Stream.fibs().take(1).toList shouldEqual List(0)
      Stream.fibs().take(2).toList shouldEqual List(0, 1)
      Stream.fibs().take(6).toList shouldEqual List(0, 1, 1, 2, 3, 5)
    }
  }

  describe("Stream.unfold") {
    it("generates a stream using given function to produce elements") {
      Stream.unfold(0) { s => Some((s + 1, s + 1)) }.take(3).toList shouldEqual List(1, 2, 3)
    }

    it("stops stream generation when generator function returns None") {
      Stream.unfold(0) { s =>
        if (s < 3) Some((s + 1, s + 1)) else None
      }.take(5).toList shouldEqual List(1, 2, 3)
    }

    describe("ones in terms of unfold") {
      it("returns stram of ones") {
        Stream.onesU().take(3).toList shouldEqual List(1, 1, 1)
      }
    }

    describe("constant in terms of unfold") {
      it("returns stram of constants") {
        Stream.constantU(2).take(3).toList shouldEqual List(2, 2, 2)
      }
    }

    describe("from in terms of unfold") {
      it("returns stram of integers starting from given n") {
        Stream.fromU(1).take(3).toList shouldEqual List(1, 2, 3)
      }
    }

    describe("fibs in terms of unfold") {
      it("returns infinite stream of Fibonacci numbers") {
        Stream.fibsU().take(1).toList shouldEqual List(0)
        Stream.fibsU().take(2).toList shouldEqual List(0, 1)
        Stream.fibsU().take(6).toList shouldEqual List(0, 1, 1, 2, 3, 5)
      }
    }
  }
}
