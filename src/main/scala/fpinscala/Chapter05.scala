package fpinscala.Chapter05

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List[A]()
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => cons(h(), t().take(n - 1))
    case _ => empty
  }

  // Exercise 5.2
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if (n > 0) => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
    case _ => true
  }

  // Exercise 5.5
  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, acc) =>
      if (p(a)) cons(a, acc) else empty
    }

  // Exercise 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A]) { (h, _) =>
      Some(h)
    }

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B]) { (a, acc) =>
      cons(f(a), acc)
    }

  // Exercise 5.7
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]) { (a, acc) =>
      if (p(a)) cons(a, acc) else acc
    }

  // Exercise 5.7
  def append[B >: A](stream: Stream[B]): Stream[B] =
    foldRight(stream) { (a, acc) =>
      cons(a, acc)
    }

  // Exercise 5.7
  def flatmap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B]) { (a, acc) =>
      f(a).foldRight(acc) { (b, bAcc) =>
        cons(b, bAcc)
      }
    }

  // Exercise 5.13
  def mapU[B](f: A => B): Stream[B] =
    unfold(this) { stream =>
      stream match {
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }
    }

  // Exercise 5.13
  def takeU(n: Int): Stream[A] =
    unfold((this, 0)) { state =>
      val stream = state._1
      val takenSoFar = state._2

      stream match {
        case Cons(h, t) if (takenSoFar < n) => Some((h(), (t(), takenSoFar + 1)))
        case _ => None
      }
    }

  // Exercise 5.13
  def takeWhileU(p: A => Boolean): Stream[A] =
    unfold(this) { stream =>
      stream match {
        case Cons(h, t) if (p(h())) => Some((h(), t()))
        case _ => None
      }
    }

  // Exercise 5.13
  def zipWith[B >: A](other: Stream[B])(f: (B, B) => B): Stream[B] = {
    unfold((this: Stream[B], other)) { state =>
      val stream1: Stream[B] = state._1
      val stream2: Stream[B] = state._2

      (stream1, stream2) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case (Empty, _) => None
        case (_, Empty) => None
        case (_, _) => None
      }
    }
  }

  // Exercise 5.13
  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, other)) { state =>
      val stream1: Stream[A] = state._1
      val stream2: Stream[B] = state._2

      (stream1, stream2) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((
          (Some(h1()), Some(h2())),
          (t1(), t2())
        ))
        case (Empty, Cons(h2, t2)) => Some((
          (None, Some(h2())),
          (Empty, t2())
        ))
        case (Cons(h1, t1), Empty) => Some((
          (Some(h1()), None),
          (t1(), Empty)
        ))
        case (_, _) => None
      }
    }
  }

  // Exercise 5.14
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s)
      .takeWhile { pair => !pair._2.isEmpty }
      .forAll { pair => pair._1 == pair._2 }

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold(Some(this): Option[Stream[A]]) { maybeStream =>
      maybeStream match {
        case Some(Cons(h, t)) => Some((maybeStream.get, Some(t())))
        case Some(Empty) => Some((Empty, None))
        case _ => None
      }
    }

  // Exercise 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    this match {
      case Cons(h,t) => {
        val intermediateResult = t().scanRight(z)(f)
        cons(f(h(), intermediateResult.headOption.getOrElse(z)), intermediateResult)
      }
      case _ => cons(z, empty)
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Exercise 5.10
  def fibs(): Stream[Int] = {
    def f(f1: Int, f2: Int): Stream[Int] =
      cons(f1, f(f2, f1 + f2))

    f(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }

  // Exercise 5.12
  def onesU(): Stream[Int] = unfold(1)(x => Option((x, x)))

  // Exercise 5.12
  def constantU[A](a: A): Stream[A] = unfold(a)(x => Option((x, x)))

  // Exercise 5.12
  def fromU(n: Int): Stream[Int] = unfold(n - 1)(x => Some((x + 1, x + 1)))

  // Exercise 5.12
  def fibsU(): Stream[Int] =
    unfold((0, 1)){ prev =>
      Some((prev._1, (prev._2, prev._1 + prev._2)))
    }
}
