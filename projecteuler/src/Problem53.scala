import scala.annotation.tailrec

object Problem53 extends App {
  var factorial: BigInt => BigInt = (x: BigInt) => {
    @tailrec def loop(x: BigInt, acc: BigInt = 1): BigInt = {
      if (x <= 1) acc
      else loop(x - 1, x * acc)
    }
    loop(x)
  }
  def comb(n:BigInt, r: BigInt) = {
    factorial(n) / (factorial(r) * factorial( n - r) )
  }
  var counter = 0
  (23 to 100).foreach {
    i => {
      (1 to i).foreach {
        j => {
          if (comb(i, j) > 1000000) {
            counter += 1
          }
        }
      }
    }
  }
  println("counter = "+counter)
}
