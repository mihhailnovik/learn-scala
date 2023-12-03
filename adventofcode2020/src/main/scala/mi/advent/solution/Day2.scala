package mi.advent.solution

object Day2 extends Day(2) {

  //part 1
  val repeatCounters = lines.map(_.groupBy(identity).mapValues(_.length)).foldLeft((0, 0))((acc, entry) => {
    def exactly(n: Int, acc: Int) = if (entry.values.exists(_ == n)) acc + 1 else acc

    (exactly(2, acc._1), exactly(3, acc._2))
  }
  )
  val part1Solution = repeatCounters._1 * repeatCounters._2

  //part 2
  def diff(s: String, s2: String) = s.zip(s2).filter(a => a._1 != a._2)

  val almostIdentical: Option[List[String]] = lines.combinations(2).find(a => diff(a.head, a(1)).length == 1)

  val part2Solution = almostIdentical.map(a => a.head.zip(a(1)).filter(a => a._1 == a._2).map(_._1).toList.mkString)
  println(part2Solution)

  printSolution
}
