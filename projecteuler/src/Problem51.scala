object Problem51 extends App {

  val primes = 2 #:: LazyList.from(3,2).filter(isPrime)
  def isPrime(n: Int): Boolean = primes.takeWhile(p => p*p <= n).forall(n % _ != 0)

  def maxPrimeValues(n:String):(String, Int) = {
    n.toSeq.distinct.map(_.toString).map(nr => (nr, primeValues(n, nr))).maxBy(_._2)
  }
  def primeValues(number:String, char:String):Int = {
    if (!number.isEmpty) ('1' to '9').count(n => isPrime(number.replaceAll(char, n.toString).toInt)) else 0
  }
  println(s"Solution is ${primes.find(nr => maxPrimeValues(nr.toString)._2 == 8)}")
}
