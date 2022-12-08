class Day6 extends AoCPuzzle {
  override def dayNr: Int = 6
  override def active(): Boolean = false
  private val dataStream = getData().head

  private def findMarker(size: Int): Int = dataStream.sliding(size).find(_.toSet.size == size).map(dataStream.indexOf(_) + size).getOrElse(-1)

  override def part1Answer(): String = findMarker(4).toString

  override def part2Answer(): String = findMarker(14).toString
}
