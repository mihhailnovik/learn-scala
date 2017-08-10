package fp

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List extends App { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => {
        f(x, foldRight(xs, z)(f))
      }
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  //  EXERCISE 2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  //  EXERCISE 3
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    n match {
      case a if a < 1 => l
      case 1 => tail(l)
      case _ => drop(tail(l), n -1)
    }
  }

  //EXERCISE 4:
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, _) => if (f(x)) dropWhile(tail(l), f) else l
    }
  }

  //  EXERCISE 5
  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  //  EXERCISE 6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => List()
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  // EXERCISE 9
  def length[A](l: List[A]): Int = {
    @tailrec
    def lengthAcc[A](l: List[A], acc: Int): Int = {
      l match {
        case Nil => acc
        case Cons(_, xs) => lengthAcc(xs, acc + 1)
      }
    }
    lengthAcc(l, 0)
  }

  // EXERCISE 10
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // EXERCISE 11
  def reverse[A](l: List[A]): List[A] =
  l match {
    case Nil => Nil
    case Cons(a, xs) => append(reverse(xs), List(a))
  }

  def reverseFold[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((a: List[A], b: A) => Cons(b, a))
  }


  // EXERCISE 14
  def appendR[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((a, b) => Cons(a, b))
  }

  // EXERCISE 15
  def flatten[A] (l : List[List[A]]) : List[A] = {
    foldRight(l, List[A]())(appendR)
  }

  // EXERCISE 18
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

  // EXERCISE 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, List[A]())((a, b) => if (f(a)) b else Cons(a, b))
  }

  // EXERCISE 20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    flatten(map(l)(f))
  }

  // EXERCISE 21
  def filterFlat[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else List())
  }

  // EXERCISE 22-23
  def zip[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = {
    (a, b) match {
      case (Nil, Nil) => Nil
      case (Cons(x1, xs1), Cons(y1, ys1)) => Cons(f(x1, y1), zip(xs1, ys1)(f))
    }
  }

  // EXERCISE 24
  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    @tailrec
    def fullMatch(l: List[A], sub: List[A]): Boolean = {
      (l, sub) match {
        case (_, Nil) => true
        case (Cons(h, t), Cons(x1, xs1)) => h == x1 && fullMatch(t, xs1)
      }
    }
    (l, sub) match {
      case (Nil, _) => sub == Nil
      case (Cons(h, t), Cons(x1, xs1)) => if (h != x1) hasSubsequence(t, Cons(x1, xs1) ) else fullMatch(t, xs1)
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t : Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(a,b) => 1 + size(a) + size(b)
    }
  }

  def max(t: Tree[Int]): Int = {
    t match {
      case Leaf(a) => a
      case Branch(a, b) => max(a).max(max(b))
    }
  }

  def maxDepth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(a, b) => 1 + maxDepth(a).max(maxDepth(b))
    }
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(a, b) => map(Branch(a, b), f)
    }
  }

}
