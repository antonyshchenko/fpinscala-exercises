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
}
