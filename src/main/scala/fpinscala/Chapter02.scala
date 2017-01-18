package fpinscala

object Chapter02 {
  // Excercise 2.1
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

  // Excercise 2.2
  // Implement isSorted which checks wheter an Array[A] is sorted
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
}
