import S99Int._

import scala.annotation.tailrec
object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  def sieve(s: Stream[Int]): Stream[Int] = {
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  }
  // All primes as a lazy sequence
  val primes: Stream[Int] = sieve(Stream.from(2))
}



class S99Int(val start: Int) {

  def isPrime: Boolean = primes.takeWhile(start >= _).contains(start) // 31
  def isCoprimeTo(i: Int): Boolean = Arithmetic_31to41.gcd(start, i) == 1 // 33
  def totient: Int = (1 to start).count(_.isCoprimeTo(start)) // 34

  // 35
  def primeFactors: List[Int] = {
    @tailrec def primeFactorAcc(nr: Int, currentDiv: Int, limit: Int, acc: List[Int]): List[Int] = {
      if (currentDiv > limit) {
        acc
      }
      else if (nr % currentDiv == 0) {
        primeFactorAcc(nr / currentDiv, currentDiv, limit, acc :+ currentDiv)
      } else {
        primeFactorAcc(nr, currentDiv + 1, limit, acc) // can optimize and increase by 2 if number is odd
      }
    }

    primeFactorAcc(start, 2, start / 2, List[Int]())
  }

  // 36
  def primeFactorMultiplicity: Map[Int, Int] = {
    def primeFactorAcc(factors: List[Int], acc: Map[Int, Int]): Map[Int, Int] = {
      factors match {
        case Nil => acc
        case x :: xs => primeFactorAcc(xs, acc updated(x, acc.getOrElse(x, 0) + 1))
      }
    }

    primeFactorAcc(primeFactors, Map[Int, Int]())
  }

  // 37
  def phi: Int = {
    primeFactorMultiplicity.map(a => (a._1 - 1) * Math.pow(a._1 - 1, a._2 - 1)).product.toInt
  }

  // 40
  def goldbach: (Int, Int) = {
    primes.takeWhile(_ < start).find(i => (start - i).isPrime) match {
      case None => throw new IllegalArgumentException
      case Some(p1) => (p1, start - p1)
    }
  }
}

object Arithmetic_31to41 extends App {
  // 31
  11.isPrime

  // 32
  def gcd(i:Int, y:Int): Int = {
    if (i == 0) y
    else if (y == 0) i
    else if (i > y)
      gcd(i % y, y)
    else
      gcd(y % i, i)
  }

  // 33
  35.isCoprimeTo(64)
  // 34
  10.totient
  // 35
  315.primeFactors
  // 36
  315.primeFactorMultiplicity
  // 37
  10.phi
  // 38
  10090.phi == 10090.totient
  // 39
  def listPrimesinRange(range: Range) = primes.filter(_ >= range.head).takeWhile(_ <= range.last).toList
  // 40
  28.goldbach
  // 41
  def printGoldbachList(range: Range): Unit = printGoldbachListLimited(range, 0)
  def printGoldbachListLimited(range: Range, i: Int): Unit = {
    range.filter(n => n > 2 && n % 2 == 0).map(n => (n, n.goldbach)).filter(_._2._1 >= i) foreach {
      case (n, (p1, p2)) => println(s"$n = $p1 + $p2")
    }
  }
}


