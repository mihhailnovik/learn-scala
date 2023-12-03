package mi.advent.solution

import scala.collection.immutable.{Set, Stream}

object Day1 extends Day(1) {
  val data: List[Int] = lines.map(_.toInt)

  def firstDup[T](stream: Stream[T], seen: Set[T] = Set.empty[T]): T = stream match {
    case head #:: _ if seen(head) => head
    case head #:: tail => firstDup(tail, seen + head)
  }

  val part1Solution = data.sum
  val part2Solution = firstDup(Stream.continually(data).flatten.scanLeft(0)(_ + _))

  printSolution
}