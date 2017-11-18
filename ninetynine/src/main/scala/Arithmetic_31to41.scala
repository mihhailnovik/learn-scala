import S99Int._
object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}

class S99Int(val start: Int) {
  private def primes: Stream[Int] = {
    val stream = Stream.from(3, 2)
    1 #:: 2 #:: stream.head #:: stream.tail.filter(_ % stream.head != 0)
  }

  def isPrime: Boolean = primes.takeWhile(start >= _).contains(start) // 31
  def isCoprimeTo(i: Int): Boolean = Arithmetic_31to41.gcd(start, i) == 1 // 33
  def totient: Int = (1 to start).count(_.isCoprimeTo(start)) // 34

  // 35
  def primeFactors: List[Int] = {
    def primeFactorAcc(nr: Int, currentDiv: Int, limit: Int, acc: List[Int]): List[Int] = {
      if (currentDiv > limit) {
        acc
      }
      else if (nr % currentDiv == 0) {
        primeFactorAcc(nr / currentDiv, currentDiv, limit, acc :+ currentDiv)
      } else {
        primeFactorAcc(nr, currentDiv + 1, limit, acc) // can optimize and increase by 2 if number is odd
      }
    }

    primeFactorAcc(start, 2, Math.sqrt(start).toInt, List[Int]())
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
  //35
  println(315.primeFactors)
}


