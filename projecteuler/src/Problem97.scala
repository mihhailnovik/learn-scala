

object Problem97 extends App {
  val div = BigInt("10000000000")
  def slice(nr:BigInt): BigInt = (nr /% div)._2

  def pow2(nr:BigInt, p:BigInt):BigInt = if (p == 0) nr else pow2(slice((2 * nr)), p -1)
  println("Answer is = "+slice((pow2(1, 7830457) * 28433) + 1))

}


