import scala.util.matching.Regex

class Day3 extends AoCPuzzle {

  override def active(): Boolean = true

  override def dayNr: Int = 3

  override def part1Answer(): String =
    extractMulSum(getData.mkString).toString


  override def part2Answer(): String =
    extractBetweenDoAndDont("do()"+getData.mkString+"don't()").map(extractMulSum).sum.toString

  private def extractBetweenDoAndDont(str: String): List[String] =
      """do\(\)(.*?)don't\(\)""".r.findAllMatchIn(str).map(_.group(1)).toList


  private def extractMulSum(data: String) =
    """mul\((\d+),\s*(\d+)\)""".r.findAllMatchIn(data).map { m =>
      (m.group(1).toInt, m.group(2).toInt)
    }.map(i => i._1 * i._2).sum
}
