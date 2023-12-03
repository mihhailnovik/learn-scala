package mi.advent.solution

import scala.annotation.tailrec

object Day5 extends Day(5) {

  val data = lines.head.toCharArray.toList

  @tailrec def filtering(data: List[Char]): List[Char] = {
    val initialSize = data.length
    val filteredData = removeReacts(data)
    if (filteredData.length != initialSize) {
      filtering(filteredData)
    } else {
      val head = filteredData.head
      val filteredTail = removeReacts(filteredData.tail)
      if (filteredTail.length != filteredData.tail.length) {
        filtering(head :: filteredTail)
      } else {
        head :: filteredTail
      }
    }
  }

  def react(c1: Char, c2: Char): Boolean = {
    if (c1.isLower == c2.isLower) false else c1.toLower.equals(c2.toLower)
  }

  private def removeReacts(data: List[Char]) = {
    data.grouped(2).filter(el => el.length == 1 || (el.length == 2 && !react(el.head, el(1)))).flatten.toList
  }

  override def part1Solution = filtering(data).length

  override def part2Solution = ('a' to 'z').map(c1 => data.filter(c1 != _).filter(c1 != _.toLower)).map(filtering(_).length).min

  printSolution
}
