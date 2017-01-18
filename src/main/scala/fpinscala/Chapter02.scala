package fpinscala

object Chapter02 {
  // Excercise 2.1:
  // Write a function to compute the nth fibonacci number
  // should use local tail-recursive function
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev1: Int, prev2: Int): Int = {
      if (n == 0) prev1
      else go(n - 1, prev2, prev1 + prev2)
    }

    go(n, 0, 1)
  }

  // Excercise 2.2:
  // Implement `isSorted` which checks wheter an Array[A] is sorted
  // according to a given comparison function
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean = {
      if (i >= as.length) true
      else if (!ordered(as(i - 1), as(i))) false
      else go(i + 1)
    }

    go(1)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 2.3:
  // Implement `curry`. It should convert a function `f` of two arguments into
  // a function of one argument that partially applies `f`.
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => partial1(a, f)

  // Exercise 4:
  // Implement `uncurry`, which reverses transformation of `curry`.
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // Exercise 5:
  // Implement `compose`
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
