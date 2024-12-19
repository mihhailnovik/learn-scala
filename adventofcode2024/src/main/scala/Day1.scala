class Day1 extends AoCPuzzle {

  override def dayNr: Int = 1

  override def active(): Boolean = true

  override def part1Answer(): String =
    val (list1, list2) = loadLists
    list1.zip(list2).map((a, b) => Math.abs(a - b)).sum.toString

  override def part2Answer(): String =
    val (list1, list2) = loadLists
    list1.map(el => list2.count(_ == el) * el).sum.toString

  private def loadLists =
    val data = getData.map(_.split(" {3}"))
    val list1 = data.map(_.head.toInt).sorted
    val list2 = data.map(_.tail.head.toInt).sorted
    (list1, list2)
}
