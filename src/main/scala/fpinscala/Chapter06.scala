package fpinscala.Chapter05

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (x, state) if x >= 0 => (x, state)
      case (Int.MinValue, state) => (Int.MaxValue, state)
      case (x, state) if x < 0 => (-x, state)
    }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val randInt = nonNegativeInt(rng)
    (randInt._1.toDouble / Int.MaxValue, randInt._2)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (randomInt, _) = rng.nextInt
    val (randomDouble, rng2) = double(rng)
    ((randomInt, randomDouble), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (randomInt, _) = rng.nextInt
    val (randomDouble, rng2) = double(rng)
    ((randomDouble, randomInt), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val res = Range(0, count).toList.foldRight((List[Int](), rng)) { (_, state) =>
      val (nextRandInt, nextRng) = state._2.nextInt
      (nextRandInt :: state._1, nextRng)
    }
    res.copy(res._1.reverse)
  }

  // Exercise 6.5
  def doubleM(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt) { randInt =>
      randInt.toDouble / Int.MaxValue
    }.apply(rng)
  }

  // Exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      val res = fs.foldLeft((List[A](), rng)) { (state, f) =>
        val (a, nextRng) = f(state._2)
        (a :: state._1, nextRng)
      }
      res.copy(res._1.reverse)
    }
  }

  // Exercise 6.7
  def intsUsingSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  // Exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  // Exercise 6.8
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {i: Int =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
