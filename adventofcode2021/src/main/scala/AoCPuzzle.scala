import scala.io.{BufferedSource, Source}

trait AoCPuzzle {
  def dayNr: Int

  def getData(): List[String] = Source.fromResource("day" + dayNr + ".txt").getLines().toList

  def part1Answer(): String
  def part2Answer(): String
}
