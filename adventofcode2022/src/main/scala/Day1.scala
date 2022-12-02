class Day1 extends AoCPuzzle {
  override def dayNr: Int = 1
  override def active(): Boolean = false

  private val elvesFoodTotalAmount: Array[Int] = getData().map {
    case "" => "-"
    case any => any
  }.mkString(",").split(",-,").map(_.split(",").map(_.toInt)).map(_.sum)
  override def part1Answer() = elvesFoodTotalAmount.max.toString
  override def part2Answer() = elvesFoodTotalAmount.sorted.reverse.take(3).sum.toString

}
