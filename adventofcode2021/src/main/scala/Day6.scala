import scala.collection.mutable

object Day6 extends App with AoCPuzzle {
  override def dayNr = 6
  val cache = mutable.HashMap[(Int, Int), Long]()

  override def part1Answer() = simulateFishes(getData().head.split(",").map(_.toInt).toList, 80).toString

  override def part2Answer() = simulateFishes(getData().head.split(",").map(_.toInt).toList, 256).toString

  def simulateFishes(elems: List[Int], days: Int): Long = {
    elems.map(fish => countFishesDynamic(fish, days)).sum
  }

  def countFishesDynamic(fish: Int, days: Int): Long = cache.getOrElseUpdate((fish, days), {
    if (days == 0) 1
    else {
      if (fish == 0) countFishesDynamic(6, days - 1) + countFishesDynamic(8, days - 1) else
        countFishesDynamic(fish - 1, days - 1)
    }
  })
  print(part2Answer())
}