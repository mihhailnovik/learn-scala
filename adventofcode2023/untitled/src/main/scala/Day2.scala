import scala.util.matching.Regex

class Day2 extends AoCPuzzle {
  override def dayNr: Int = 2

  val pattern: Regex = "Game (\\d+):".r

  private def parse(str: String): Map[String, Int] = str.split(": ")(1).split(";").map { segment =>
    segment.trim.split(", ").flatMap { pair =>
      val parts = pair.split(" ")
      if (parts.length == 2) Some(parts(1), parts(0).toInt) else None
    }.toMap
  }.toList.flatMap(_.toList).groupMapReduce(_._1)(_._2)(math.max) // Convert the final array to List

  def gameIdIfPossible(str: String): Int = {
    val id = pattern.findFirstMatchIn(str).get.group(1).toInt
    val result = parse(str)
    if result.getOrElse("red", 0) <= 12 && result.getOrElse("green", 0) <= 13 && result.getOrElse("blue", 0) <= 14
    then id else 0
  }

  def powerOfMinimum(str: String): Int = {
    val result: Map[String, Int] = parse(str)
    result.values.product
  }

  override def part1Answer(): String = {
    getData().map(gameIdIfPossible).sum.toString
  }

  override def part2Answer(): String = {
    getData().map(powerOfMinimum).sum.toString
  }

}
