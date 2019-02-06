package mi.advent.solution

import scala.io.Source

abstract class Day(day: Int) extends App {
  private val fileLocation = s"/home/mihhailnovik/programming/scala/learn-scala/adventofcode/src/main/resources/day$day.txt"
  protected val source = Source.fromFile(fileLocation)
  protected val lines: List[String] = source.getLines.toList


  def part1Solution: Any

  def part2Solution: Any

  protected def printSolution(): Unit = println("Part A = " + part1Solution + "\n" +
    "Part B = " + part2Solution)

  def testData = ""
}
