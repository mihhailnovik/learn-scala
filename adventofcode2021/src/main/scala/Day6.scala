import scala.collection.mutable

object Day6 extends App with AoCPuzzle {
  override def dayNr = 6

  override def part1Answer() = simulateFishes(getData().head.split(",").map(_.toInt).toList, 80).toString

  override def part2Answer() = simulateFishes(getData().head.split(",").map(_.toInt).toList, 256).toString

  val cache = mutable.HashMap[(Int, Int), Long]()

  def simulateFishes(elems: List[Int], days: Int) : Int = {
    if (days == 0) elems.length
    else {
      val newFishes = elems.count(_ == 0)
      val newElements = elems.map(nr => if (nr == 0) 6 else nr -1) ::: List.fill(newFishes)(8)
      simulateFishes(newElements, days -1)
    }
  }

  def countFishes(fish: Int, days: Int): Long = {
    cache.getOrElseUpdate((fish, days), {
      if (days == 0) 1 else {
        if (fish == 0) 1 + countFishes(6, days - 1) + countFishes(8, days -1) else countFishes(fish -1, days -1)
      }
    })
  }


  print("answer = "+ countFishes(1, 9) )



}
