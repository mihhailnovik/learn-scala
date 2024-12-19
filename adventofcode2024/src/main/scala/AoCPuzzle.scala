import scala.io.Source

trait AoCPuzzle {
  def dayNr: Int

  def getData: List[String] = Source.fromResource(dayNr + ".txt").getLines().toList

  def part1Answer(): String
  def part2Answer(): String

  def active(): Boolean = false
}
