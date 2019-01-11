package mi.advent.solution

import scala.collection.immutable.{Set, Stream}
import scala.collection.mutable
import scala.io.Source

object Day1 extends App {
  val fileLocation = "/home/mihhailnovik/programming/scala/learn-scala/adventofcode/src/main/resources/day1.txt"
  val data: List[Int] = Source.fromFile(fileLocation).getLines.map(_.toInt).toList
  val results = mutable.HashSet(0)

  def firstDup[T](stream: Stream[T], seen: Set[T] = Set.empty[T]): T = stream match {
    case head #:: _ if seen(head) => head
    case head #:: tail => firstDup(tail, seen + head)
  }

  val part1Solution = data.sum
  val part2Solution = firstDup(Stream.continually(data).flatten.scanLeft(0)(_ + _))

  println(part2Solution)
}