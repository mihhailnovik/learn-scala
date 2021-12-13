import Day2.Command

object Day2 extends App with AoCPuzzle {
  override def dayNr = 2

  val data = getData().map(data => new Command(data))

  override def part1Answer(): String = {
    val result = calculateSimpleMovement(data)
    (result.hor * result.depth).toString
  }

  override def part2Answer(): String = {
    val result = calculateMovementWithAim(data)
    (result.hor * result.depth).toString
  }

  private def calculateSimpleMovement(data: List[Command]) = {
    data.foldLeft(Coordinate(0, 0, 0)) {
      case (coordinate, Command("forward", value)) => coordinate.copy(hor = coordinate.hor + value)
      case (coordinate, Command("up", value)) => coordinate.copy(depth = coordinate.depth - value)
      case (coordinate, Command("down", value)) => coordinate.copy(depth = coordinate.depth + value)
    }
  }

  private def calculateMovementWithAim(data: List[Command]) = {
    data.foldLeft(Coordinate(0, 0, 0)) {
      case (coordinate, Command("forward", value)) => coordinate.copy(hor = coordinate.hor + value, depth = coordinate.depth + coordinate.aim * value)
      case (coordinate, Command("up", value)) => coordinate.copy(aim = coordinate.aim - value)
      case (coordinate, Command("down", value)) => coordinate.copy(aim = coordinate.aim + value)
    }
  }

  case class Coordinate(hor: Int, depth: Int, aim: Int)

  case class Command(action: String, value: Int) {
    def this(dataRow: String) = this(dataRow.split(" ").head, dataRow.split(" ").tail.head.toInt)
  }

}
