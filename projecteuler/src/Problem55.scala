object Problem55 extends App {

  def isLychrel(curNr:BigInt, iterLeft: BigInt):Boolean =
    (iterLeft == 0) || (!isPalindromic(curNr) & isLychrel(curNr + BigInt(curNr.toString.reverse), iterLeft - 1))

  def isPalindromic( v:BigInt ) = { val s = (v + BigInt(v.toString.reverse)).toString;s == s.reverse}

  val result = LazyList.from(1).take(10000).map(BigInt(_)).count(n => isLychrel(n, 50))

  println(s"Answer is $result")
}
