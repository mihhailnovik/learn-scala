import scala.annotation.tailrec

object Problem57 extends App {

  def gcd(a: Int, b: Int):Int=if (b==0) a.abs else gcd(b, a%b)
  def lcm(a: Int, b: Int)=(a*b).abs/gcd(a,b)

  object Fraction {
    def apply(num: Int, denom: Int): Fraction = new Fraction(num, denom)
    def apply(num: Int, other: Fraction): Fraction = Fraction(num * other.denom, other.num)
  }

  case class Fraction(num: Int, denom: Int) {
    def sum(other: Fraction) = {
      val commonMultiplier = lcm(other.denom, denom)
      Fraction(commonMultiplier / denom * num + commonMultiplier / other.denom * other.num, commonMultiplier)
    }
  }


  def expansion(depth: Int):Fraction = {

    if (depth == 0) (Fraction(1, 2))
    else Fraction(1, (Fraction(2, 1).sum(expansion(depth - 1))))
  }

  val answer = LazyList.from(0).take(999).map(expansion).count(r => r.num.toString.length > r.denom.toString.length)
  println("answer "+answer)

}
