import scala.collection.mutable

object Day7 extends App with AoCPuzzle {
  override def dayNr = 7
  val processedData = getData().head.split(",").map(_.toInt)

  override def part1Answer() = {
    val median: Int = processedData.sorted(Ordering.Int)(processedData.length / 2)
    processedData.map(number => constantRateDistance(number, median)).sum.toString
  }


  override def part2Answer() = {
    val average: Int = processedData.sum / processedData.length
    val sum1 = processedData.map(number => increaseRateDistance(number, average)).sum
    val sum2 = processedData.map(number => increaseRateDistance(number, average+1)).sum
    val sum3 = processedData.map(number => increaseRateDistance(number, average-1)).sum
    Math.min(sum3, Math.min(sum1, sum2)).toString // good enough :)
  }


  private def constantRateDistance(x1: Int, x2: Int) =  Math.abs(x1 - x2)
  private def increaseRateDistance(x1: Int, x2: Int) = (1 to Math.abs(x1 - x2)).toList.sum


  println(part2Answer())
}