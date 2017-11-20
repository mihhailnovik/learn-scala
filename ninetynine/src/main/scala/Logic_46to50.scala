object Logic_46to50 extends App {

  //and, or, nand, nor, xor, impl, and equ

  def not(a: Boolean) = a match {
    case true => false
    case false => true
  }

  def and(a: Boolean, b: Boolean) = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean) = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case _ => false
  }

  def nand(a: Boolean, b: Boolean) = not(and(a, b))

  def nor(a: Boolean, b: Boolean) = not(or(a, b))

  def xor(a: Boolean, b: Boolean) = a && not(b) || b && not(a)

  def impl(a: Boolean, b: Boolean) = or(not(a), b)

  def equ(a: Boolean, b: Boolean) = or(and(a, b), and(not(a), not(b)))

  // 46
  def table2(logOp: (Boolean, Boolean) => Boolean) = {
    println("A     B     result")
    val boolList = List[Boolean](true, false)
    for {b1 <- boolList
         b2 <- boolList} {
      println(b1 + "     " + b2 + "     " + logOp(b1, b1))
    }
  }
  table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
}
