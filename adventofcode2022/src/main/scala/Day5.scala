import scala.util.Try

class Day5 extends AoCPuzzle {
  override def dayNr: Int = 5

  override def active(): Boolean = true

  case class Action(amount: Int, from: Int, to: Int)

  private def parseInput() = {
    val movePattern = "applyActions ([0-9]+) from ([0-9]+) to ([0-9]+)".r
    def enc(i: Int): Int = {
      if (i == 1) i else 4 + enc(i - 1)
    }
    val input = getData()
    val initialData = input.take(input.indexOf(""))
    val initialState = (for (i <- 1 to 9) yield initialData.flatMap {
      str => {
        val index: Int = enc(i)
        if (str.length < index) List.empty[Char] else {
          val char = str.charAt(index)
          if (char == ' ') List.empty[Char] else List(str.charAt(index))
        }
      }
    }).toList
    val moveData = input.slice(input.indexOf("") + 1, input.size).map { row =>
      val movePattern(amount, from, to) = row
      Action(amount.toInt, from.toInt, to.toInt)
    }
    (initialState, moveData)
  }


  private def applyActions(currentState: List[List[Char]], actions: List[Action], reverse: Boolean): List[List[Char]] = {
    def order(toReverse: List[Char]) = if (reverse) toReverse else toReverse.reverse

    if (actions.isEmpty) currentState else {
      val action = actions.head
      val forReplace = currentState(action.from - 1).splitAt(action.amount)
      val updatedState = currentState
        .updated(action.from - 1, forReplace._2)
        .updated(action.to - 1, order(forReplace._1).appendedAll(currentState(action.to - 1)))
      applyActions(updatedState, actions.tail, reverse)
    }
  }

  override def part1Answer(): String = {
    val input = parseInput()
    applyActions(input._1, input._2, false).map(_.head).mkString
  }

  override def part2Answer(): String = {
    val input = parseInput()
    applyActions(input._1, input._2, true).map(_.head).mkString
  }

}
