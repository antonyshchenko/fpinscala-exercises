package fpinscala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Can't return tail of empty list")
      case Cons(x, xs) => xs
    }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("Can't set head of empty list")
      case Cons(x, xs) => Cons(h, xs)
    }

  // Exercise 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) {
      l
    } else {
      l match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }
    }

  // Exercise 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
      case _ => l
    }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Can't init empty list")
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  // Exercise 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  // Exercise 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(l: List[A], acc: B): B =
      l match {
        case Nil => acc
        case Cons(x, xs) => go(xs, f(acc, x))
      }
    go(l, z)
  }

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((element, acc) => Cons(f(element), acc))
  }
}
