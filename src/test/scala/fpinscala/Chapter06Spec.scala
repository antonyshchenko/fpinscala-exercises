package fpinscala.Chapter05

import org.scalatest._

class Chapter06Spec extends FunSpec with Matchers {
  describe("RNG") {
    describe("nonNegativeInt") {
      it("returns non negative random int") {
        RNG.nonNegativeInt(RNG.Simple(1))._1 shouldEqual 384748
        RNG.nonNegativeInt(RNG.Simple(13123464))._1 shouldEqual 1643200585
      }
    }

    describe("double") {
      it("random double betweeon 0 and 1") {
        RNG.double(RNG.Simple(1))._1 shouldEqual 1.79162249052507E-4
        RNG.double(RNG.Simple(131234641))._1 shouldEqual 0.34843016664890114
      }
    }

    describe("intDouble") {
      it("returns a pair of random int and random double") {
        RNG.intDouble(RNG.Simple(1))._1 shouldEqual (384748, 1.79162249052507E-4)
      }
    }

    describe("doubleInt") {
      it("returns a pair of random double and random int") {
        RNG.doubleInt(RNG.Simple(1))._1 shouldEqual (1.79162249052507E-4, 384748)
      }
    }

    describe("double3") {
      it("returns a tuple of 3 random doubles") {
        RNG.double3(RNG.Simple(1))._1 shouldEqual (1.79162249052507E-4,0.5360936464444239,0.2558267895392267)
      }
    }

    describe("ints") {
      it("returns a list of random integers") {
        RNG.ints(3)(RNG.Simple(1))._1 shouldEqual List(-549383847, -1151252339, 384748)
      }
    }

    describe("double implemented with map") {
      it("random double betweeon 0 and 1") {
        RNG.doubleM(RNG.Simple(1))._1 shouldEqual 1.79162249052507E-4
        RNG.doubleM(RNG.Simple(131234641))._1 shouldEqual 0.34843016664890114
      }
    }

    describe("map2") {
      it("returns a result of combining 2 rand operations") {
        RNG.map2(RNG.nonNegativeInt, RNG.nonNegativeInt)(_ + _).apply(RNG.Simple(1))._1 shouldEqual 384748 + 1151252339
      }
    }
  }
}
