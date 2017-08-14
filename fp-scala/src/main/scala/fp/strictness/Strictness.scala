package fp.strictness

import fp.strictness.Strictness.Stream.{cons, empty, unfold}

object Strictness extends App {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty

    override def toString: String = "(" + uncons + ")"

    // EXERCISE 1
    def toList: List[A] = this.uncons match {
      case Some((h, t)) => h :: t.toList
      case _ => List[A]()
    }

    // EXERCISE 2
    def take(n: Int): Stream[A] = n match {
      case 1 => cons(uncons.get._1, empty)
      case _ =>  cons(uncons.get._1, uncons.get._2.take(n -1))
    }

    // EXERCISE 3
    def takeWhile(p: A => Boolean): Stream[A] = {
      if (this.isEmpty) {
        return empty
      }
      if (p(uncons.get._1)) {
        cons(uncons.get._1, uncons.get._2.takeWhile(p))
      } else {
        empty
      }
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      uncons match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case None => z
      }
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    // EXERCISE 4
    def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((a, b) => p(a) && b)
    }

    // EXERCISE 5
    def takeWhileWithFold(f: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (f(a)) cons(a,b) else empty)

    // EXERCISE 6  Implement map, filter, append, and flatMap
    def mapWithFold[B](f : A => B) : Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))
    def filterWithFold(f: A => Boolean) : Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)
    def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))
    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)

      // EXERCISE 12 implement map, take, takeWhile, zip
    def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this)(s =>
      s.uncons match {
        case Some((h, t)) => Some((f(h), t))
        case _ => None
      }
    )

    def takeViaUnfold(n: Int): Stream[A] =
      unfold((this, n))(
        s => (s._1.uncons, s._2) match {
          case (Some((h, t)), n1) if n1 > 0 => Some(h, (t, n1 - 1))
          case _ => None
        }
      )

    def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
      unfold(this)(s => {
        s.uncons match {
          case Some((h, t)) if p(h) => Some((h, t))
          case _ => None
        }
      })

    def zip[B>:A](a: Stream[B])(f: (B, B) => B): Stream[B] =
      unfold(this, a) {
        case (s, s2) =>
          (s.uncons, s2.uncons) match {
            case (Some((h1, t1)), Some((h2, t2))) => Some(f(h1, h2), (t1, t2))
            case _ => None
          }
      }

  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None
      }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    // EXERCISE 7
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // EXERCISE 8
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // EXERCISE 9
    def fibs(): Stream[Int] = {
      def fibsI(firstNr: Int, lastNr: Int): Stream[Int] = {
        cons(lastNr, fibsI(lastNr, firstNr + lastNr))
      }
      cons(0, fibsI(0, 1))
    }

    // EXERCISE 10
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      if (f(z).isDefined) cons(f(z).get._1, unfold(f(z).get._2)(f)) else empty[A]
    }
    // EXERCISE 11 fibs, from, constant, and ones
    def constantFold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))
    def onesFold[A](a: A): Stream[Int] = constantFold(1)
    def fromFold(n: Int): Stream[Int] = unfold(n)(_ => Some((n, n + 1)))
    def fibFold(): Stream[Int] = unfold((0, 1))(s => Some(s._1 + s._2, (s._2, s._1 + s._2)))

  }

  println(Stream.fibFold().takeWhileViaUnfold(_ < 67))
//  println(cons(2, cons(3, cons(4, cons(5, empty)))).append(cons(1, cons(2, empty))))
}
