class Day2 extends AoCPuzzle {

  override def dayNr: Int = 2

  override def active(): Boolean = true

  override def part1Answer(): String =
    getData.map(str => str.split(" ").map(_.toInt)).count(isPureSafe).toString

  override def part2Answer(): String =
    getData.map(str => str.split(" ").map(_.toInt)).count(isSafeWithDampener).toString

  private def isPureSafe(row : Array[Int]): Boolean =
    row.sliding(2).foldLeft((row(0) > row(1), true))((counter, b) =>
      {
        counter match
          case (_, false) => (false, false)
          case (decreasing, true) =>
            (decreasing, b(0) != b(1) && (decreasing == b(0) > b(1)) && (Math.abs(b(0) - b(1)) < 4))
      })._2

  private def isSafeWithDampener(row: Array[Int]): Boolean =
    if (isPureSafe(row)) true else {
      row.indices.exists(index => {
        isPureSafe(row.slice(0, index).appendedAll(row.slice(index + 1, row.length)))
      })
    }

}
