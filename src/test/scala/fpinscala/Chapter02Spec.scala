package fpinscala

import org.scalatest._

class Chapter02Spec extends FunSpec with Matchers {
  describe("fib") {
    it("returns Fibonacci number") {
      Chapter02.fib(0) shouldEqual 0
      Chapter02.fib(1) shouldEqual 1
      Chapter02.fib(2) shouldEqual 1
      Chapter02.fib(3) shouldEqual 2
      Chapter02.fib(4) shouldEqual 3
    }
  }

  describe("isSorted") {
    it("returns true when array is sorted") {
      Chapter02.isSorted(Array(1, 2, 3), (x: Int, y: Int) => x < y) shouldEqual true
    }

    it("returns false when array is not sorted") {
      Chapter02.isSorted(Array(2, 1, 3), (x: Int, y: Int) => x < y) shouldEqual false
    }

    it("returns true when array is empty") {
      Chapter02.isSorted(Array[Int](), (x: Int, y: Int) => x < y) shouldEqual true
    }

    it("returns true when array contains only one element") {
      Chapter02.isSorted(Array(1), (x: Int, y: Int) => x < y) shouldEqual true
    }
  }

  describe("curry") {
    it("returns a curried function") {
      val plus = (x: Int, y: Int) => x + y

      val curriedPlus = Chapter02.curry(plus)

      curriedPlus(1)(2) shouldEqual 3
      curriedPlus(1)(3) shouldEqual 4
    }
  }

  describe("uncurry") {
    it("unwraps curried function into a normal function") {
      val plus = (x: Int, y: Int) => x + y

      Chapter02.uncurry(Chapter02.curry(plus))(1, 2) shouldEqual plus(1, 2)
    }
  }

  describe("compose") {
    it("composes 2 functions") {
      val f = (x: Int) => - x
      val g = (x: Int) => x * x

      Chapter02.compose(f, g)(2) shouldEqual f(g(2))
      Chapter02.compose(g, f)(2) shouldEqual g(f(2))
    }
  }
}
