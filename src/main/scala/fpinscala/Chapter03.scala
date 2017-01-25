package fpinscala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
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

  // Exercise 3.11 - sum implemented with foldLeft
  def sum(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  // Exercise 3.11 - product implemented with foldLeft
  def product(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  // Exercise 3.11 - length implemented with foldLeft
  def lengthOptimized[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, element) => Cons(element, acc))
  }

  // Exercise 3.13
  def foldRightOptimized[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((acc, element) => f(element, acc))

  // Exercise 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] =
    reverse(foldLeft(a2, reverse(a1)) { (acc, element) =>
      Cons(element, acc)
    })

  // Exercise 3.15
  // implement flatten function that has linear complexity
  def flatten[A](l: List[List[A]]): List[A] = {
    reverse(foldLeft(l, List[A]()) { (res, sublist) =>
      foldLeft(sublist, res) { (acc, element) => Cons(element, acc) }
    })
  }

  // Exercise 3.16
  def incrementByOne(l: List[Int]): List[Int] =
    reverse(foldLeft(l, List[Int]()) { (acc, element) =>
      Cons(element + 1, acc)
    })

  // Exercise 3.17
  def toListOfStrings(l: List[Double]): List[String] =
    reverse(foldLeft(l, List[String]()) { (acc, element) =>
      Cons(element.toString, acc)
    })

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    reverse(foldLeft(l, List[B]()) { (acc, element) =>
      Cons(f(element), acc)
    })
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    reverse(foldLeft(as, List[A]()) { (acc, element) =>
      if (f(element)) {
        Cons(element, acc)
      } else {
        acc
      }
    })

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    reverse(foldLeft(as, List[B]()) { (res, element) =>
      foldLeft(f(element), res) { (acc, element) => Cons(element, acc) }
    })

  // Exercise 3.21
  def filterThroughFlatmap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as) { element =>
      if (f(element))
        List(element)
      else
        List[A]()
    }

  // Exercise 3.22
  def zipWithSum(l1: List[Int], l2: List[Int]): List[Int] = {
    zipWith(l1, l2) { (el1, el2) => el1 + el2 }
  }

  // Exercise 3.23
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
    @annotation.tailrec
    def go(l1: List[A], l2: List[A], acc: List[A]): List[A] =
      (l1, l2) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(x1, xs1), Cons(x2, xs2)) => go(xs1, xs2, Cons(f(x1, x2), acc))
      }

    reverse(go(l1, l2, List[A]()))
  }
}
