class Day1 extends AoCPuzzle {
  override def dayNr: Int = 1

  private val numbers: List[Int] = (1 to 9).toList
  private val numberWords = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  // Creating a map directly from the number strings and number words to their numeric values
  private val numbersMap = numbers.map(n => n.toString -> n).toMap
  private val wordsMap = numberWords.zip(numbers).toMap
  private val numbersAndWords = numbersMap ++ wordsMap

  private def calibrationValue(str: String, dictionary: Map[String, Int]): Int = {
    val indexes = dictionary.flatMap { case (nr, value) =>
      List(str.indexOf(nr) -> value, str.lastIndexOf(nr) -> value)
    }.filter(_._1 >= 0)

    if (indexes.isEmpty) return 0

    val first = indexes.minBy(_._1)._2
    val last = indexes.maxBy(_._1)._2

    (first.toString + last.toString).toInt
  }

  override def part1Answer() = getData().map(calibrationValue(_, numbersMap)).sum.toString
  override def part2Answer() = getData().map(calibrationValue(_, numbersAndWords)).sum.toString

  private def toNumber(str: String): Int = numbersAndWords.getOrElse(str, 0)

  override def active(): Boolean = false
}