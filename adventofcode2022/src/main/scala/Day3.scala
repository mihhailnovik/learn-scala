class Day3 extends AoCPuzzle {
  override def dayNr: Int = 3

  def score(s: Char): Int = if (s.isUpper) s.toInt - 38 else s.toInt - 96

  override def part1Answer(): String = {
    getData().map(str => str.splitAt(str.length / 2)).map {
      row => row._1.find(row._2.contains).getOrElse(Char.MinValue)
    }.map(score).sum.toString
  }

  override def part2Answer(): String = {
    getData().sliding(3, 3).map {
      list => list.head.find(el => list(1).contains(el) && list(2).contains(el)).getOrElse(Char.MinValue)
    }.map(score).sum.toString
  }
}
