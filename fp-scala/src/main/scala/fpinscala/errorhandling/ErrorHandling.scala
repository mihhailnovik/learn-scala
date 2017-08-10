package fpinscala.errorhandling

import java.util.regex.{Pattern, PatternSyntaxException}

// EXERCISE 1
object ErrorHandling extends App {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] =
      this match {
        case None => None
        case a: Some[A] => Some(f(a.get))
      }

    def getOrElse[B >: A](default: => B): B =
      this match {
        case None => default
        case Some(a) => a
      }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f) getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) Some(a) else None)
  }

  case class Some[+A](get: A) extends Option[A] {
    override def filter(f: (A) => Boolean): Option[A] = if (f(get)) this else None
  }

  case object None extends Option[Nothing]

  // EXERCISE 2
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    mean(xs).flatMap(d => mean(xs.map(x => Math.pow(x - d, 2))))
  }

  // EXERCISE 3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aValue => b map (bValue => f(aValue, bValue)))
  }

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f => mkMatcher(pat2) map (g => f(s) && g(s)))

  // EXERCISE 4
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat1), mkMatcher(pat2))((a, b) => a(s) && b(s))
  }

  // EXERCISE 5
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def parsePatterns(a: List[String]): Option[List[Pattern]] =
    sequence(a map pattern)


  // EXERCISE 6
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def traverseAcc(a: List[A], acc:List[B])(f: A => Option[B]): Option[List[B]] = {
      a match {
        case Nil => Some(acc)
        case h :: t => f(h) flatMap (hh => traverseAcc(t, hh :: acc)(f))
      }
    }
    traverseAcc(a, List[B]())(f)
  }

  def sequenceTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(aa => aa)

  def parsePatterns2(a: List[String]): Option[List[Pattern]] =
    traverse(a)(pattern)

  // EXERCISE 7
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(value) => Left(value)
      case Right(value) => Right(f(value))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(value) => Left(value)
      case Right(value) => f(value)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(value) => Right(value)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for { a <- this} yield f(this,b)

  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  // EXERCISE 8: Implement sequence and traverse for Either.
  def traverseEither[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es match {
      case Nil => Right(Nil)
      case h::t =>
        (f(h) map2 traverseEither(t)(f))((aa, bb) => aa :: bb)
    }
  }

  def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverseEither(es)(x => x)



}
