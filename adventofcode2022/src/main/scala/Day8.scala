import scala.collection.mutable
import scala.collection.mutable.Map

class Day8 extends AoCPuzzle {
  override def dayNr: Int = 8

  override def active(): Boolean = true

  val input = getData().map(_.toCharArray.map(_.toString.toInt)).toArray

  case class Forrest(data: Array[Array[Int]]) {
    private val length = data.length - 1

    def value(coordinate: Coordinate): Int = data(coordinate.y.value)(coordinate.x.value)

    def isEdge(coordinate: Coordinate): Boolean = List(0, length).contains(coordinate.x.value) || List(0, length).contains(coordinate.y.value)
  }

  case class X(value: Int) {
    def left = X(value - 1)

    def right = X(value + 1)
  }

  case class Y(value: Int) {
    def top = Y(value - 1)

    def bottom = Y(value + 1)
  }

  case class Coordinate(x: X, y: Y) {
    def move(direction: Direction) = {
      direction match
        case Bottom => this.copy(x, y.bottom)
        case Top => this.copy(x, y.top)
        case Right => this.copy(x.right, y)
        case Left => this.copy(x.left, y)
        case _ => this
    }
  }

  sealed trait Direction

  object Right extends Direction

  object Left extends Direction

  object Top extends Direction

  object Bottom extends Direction

  private def isVisible(threeHeight: Int, coordinate: Coordinate, directions: List[Direction], forrest: Forrest): Boolean = {
    if (forrest.isEdge(coordinate)) true else {
      directions exists {
        direction => {
          val newCoordinate = coordinate.move(direction)
          val sideTree = forrest.value(newCoordinate)
          threeHeight > sideTree && isVisible(threeHeight, newCoordinate, List(direction), forrest)
        }
      }
    }
  }

  override def part1Answer(): String = {
    var counter = 0
    val forrest = Forrest(input)
    for (x <- input.indices) {
      for (y <- input.indices) {
        val coordinate = Coordinate(X(x), Y(y))
        val height = forrest.value(coordinate)
        if (isVisible(height, coordinate, List(Right, Left, Top, Bottom), forrest)) {
          counter += 1
        } else {
          println("NOT VISIBLE(" + input(y)(x) + ") " + x + " _ " + y)
        }
      }
    }
    counter.toString
  }

  override def part2Answer(): String =
    "???"
}
