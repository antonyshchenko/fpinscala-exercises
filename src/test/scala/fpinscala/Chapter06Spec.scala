package fpinscala.Chapter06

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
        RNG.ints(3)(RNG.Simple(1))._1 shouldEqual List(384748, -1151252339, -549383847)
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

    describe("sequence") {
      it("combines list of transitions into a single transition") {
        RNG.sequence[Int](List(RNG.nonNegativeInt, RNG.nonNegativeInt)).apply(RNG.Simple(1))._1 shouldEqual List(384748, 1151252339)
      }
    }

    describe("intsUsingSequence") {
      it("returns a list of random integers") {
        RNG.intsUsingSequence(3)(RNG.Simple(1))._1 shouldEqual List(384748, -1151252339, -549383847)
      }
    }

    describe("flatMap") {
      it("allows to generate new Rand value based on a given one") {
        val (sumOfTwoRands, _) = RNG.flatMap(RNG.nonNegativeInt) { a =>
          RNG.map(RNG.nonNegativeInt)(a + _)
        }.apply(RNG.Simple(1))

        sumOfTwoRands shouldEqual 384748 + 1151252339
      }
    }

    describe("nonNegativeLessThan") {
      it("returns non negative random integer which is less than given value") {
        val (res, _) = RNG.nonNegativeLessThan(2)(RNG.Simple(2))
        res shouldEqual 1
      }
    }

    describe("mapUsingFlatMap") {
      it("transforms rand with given function") {
        val (res, _) = RNG.mapUsingFlatMap(RNG.nonNegativeInt)(_ + 1).apply(RNG.Simple(1))
        res shouldEqual 384749
      }
    }

    describe("map2UsingFlatMap") {
      it("returns a result of combining 2 rand operations") {
        val (res, _) = RNG.map2UsingFlatMap(RNG.nonNegativeInt, RNG.nonNegativeInt)(_ + _).apply(RNG.Simple(1))
        res shouldEqual 384748 + 1151252339
      }
    }
  }

  describe("State") {
    describe("map") {
      it("transforms state") {
        State(RNG.nonNegativeInt).map(_ + 1).run(RNG.Simple(1))._1 shouldEqual 384749
      }
    }

    describe("map2") {
      it("returns a result of combining 2 state transitions") {
        val s1 = State(RNG.nonNegativeInt)
        val s2 = State(RNG.nonNegativeInt)

        val s3 = s1.map2(s2)(_ + _)

        s3.run(RNG.Simple(1))._1 shouldEqual 384748 + 1151252339
      }
    }

    describe("sequence") {
      it("combines list of state transitions into a single transition") {
        val seq = State.sequence(List(State(RNG.nonNegativeInt), State(RNG.nonNegativeInt)))

        seq.run(RNG.Simple(1))._1 shouldEqual List(384748, 1151252339)
      }
    }

    describe("flatMap") {
      it("allows to generate new state based on given one") {
        val s1 = State(RNG.nonNegativeInt)
        val s2 = State(RNG.nonNegativeInt)
        val (sumOfTwoRands, _) = s1.flatMap { a =>
          s2.map(a + _)
        }.run(RNG.Simple(1))

        sumOfTwoRands shouldEqual 384748 + 1151252339
      }
    }
  }

  describe("candy machine") {
    describe("simulateMachine") {
      it("calculates number of coins/candies for given machine state and a list of inputs") {
        Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Turn)).run(Machine(true, 5, 0)) shouldEqual ((2, 3), Machine(true, 3, 2))
      }

      it("does not dispense candies when no coin is inserted") {
        Machine.simulateMachine(List(Turn)).run(Machine(true, 5, 0)) shouldEqual ((0, 5), Machine(true, 5, 0))
      }

      it("does not unlock if coin is inserted into empty machine") {
        Machine.simulateMachine(List(Coin)).run(Machine(true, 0, 0)) shouldEqual ((0, 0), Machine(true, 0, 0))
      }
    }
  }
}
