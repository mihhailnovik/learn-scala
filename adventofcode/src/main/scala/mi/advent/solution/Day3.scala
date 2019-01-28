package mi.advent.solution

import scala.io.Source

case class Claim(id: String, x1: Int, y1: Int, x2: Int, y2: Int) {
  def coord: Seq[(Int, Int)] = for (i <- x1 to x2; y <- y1 to y2) yield (i, y)
}

object Claim {
  private val pattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

  def apply(data: String): Claim = {
    val pattern(id, left, top, width, height) = data
    Claim(id, left.toInt, top.toInt, left.toInt + width.toInt - 1, top.toInt + height.toInt - 1)
  }
}

object Day3 extends App {
  val fileLocation = "/home/mihhailnovik/programming/scala/learn-scala/adventofcode/src/main/resources/day3.txt"
  val claims: List[Claim] = Source.fromFile(fileLocation).getLines.map(Claim(_)).toList
  type Cord = (Int, Int)
  type CordToId = Map[Cord, String]
  type ResultMapAcc = (CordToId, Set[String])

  def putOrIncreaseBatch(map: ResultMapAcc, keys: List[Cord], value: String): ResultMapAcc = {
    keys match {
      case Nil => map
      case x :: xs => putOrIncreaseBatch(putOrIncrease(map, x, value), xs, value)
    }
  }

  def putOrIncrease(map: ResultMapAcc, key: Cord, value: String): ResultMapAcc = {
    map._1.get(key) match {
      case Some(e) => (map._1.updated(key, "mixed"), (map._2 + e) + value)
      case None => (map._1.updated(key, value), map._2)
    }
  }

  def overlap(claims: List[Claim], counter: ResultMapAcc): ResultMapAcc = {
    claims match {
      case Nil => counter
      case x :: xs => overlap(xs, putOrIncreaseBatch(counter, x.coord.toList, x.id))
    }
  }

  def result(claims: List[Claim]) = overlap(claims, (Map[(Int, Int), String](), Set[String]()))

  def overlaps(claims: List[Claim]): Int = {
    result(claims)._1.count(a => a._2 == "mixed")
  }

  def notOverlaped(claims: List[Claim]): String = {
    val dirtyIds = result(claims)._2
    val overlapMap = result(claims)._1
    overlapMap.filter(m => !dirtyIds.contains(m._2)).values.head
  }

  val part1Solution = overlaps(claims)
  println(part1Solution)


  val part2Solution = notOverlaped(claims)
  println(part2Solution)

}
