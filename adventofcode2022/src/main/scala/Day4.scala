class Day4 extends AoCPuzzle {
  override def dayNr: Int = 4

  override def active(): Boolean = true

  private val pattern = "([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)".r
  private val formattedInput = getData().map {
    str => {
      val pattern(r11, r12, r21, r22) = str
      (r11.toInt to r12.toInt, r21.toInt to r22.toInt)
    }
  }

  override def part1Answer(): String = {
    formattedInput.count {
      ranges => {
        ranges._1.forall(ranges._2.contains) || ranges._2.forall(ranges._1.contains)
      }
    }.toString
  }

  override def part2Answer(): String = {

    formattedInput.count {
      ranges => {
        ranges._1.exists(ranges._2.contains)
      }
    }.toString

  }

}
