import scala.io.Source

object Day1 extends App with AoCPuzzle {

  override def dayNr = 1

  val dataConverted = getData().map(_.toInt)

  override def part1Answer(): String = dataConverted.sliding(2).count(slice => slice.head < slice.tail.head).toString

  override def part2Answer(): String = dataConverted.sliding(4).count(slice => slice.take(3).sum < slice.takeRight(3).sum).toString


}

