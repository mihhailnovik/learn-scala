import scala.collection.mutable
import scala.collection.mutable.Map

class Day8 extends AoCPuzzle {
  override def dayNr: Int = 8

  private val input = getData().map(_.toCharArray.map(_.toString.toInt)).toArray
  private val forrest = Forrest(input)

  private def countVisibleTrees(treeHeight: Int, coordinate: Coordinate, direction: Direction, forrest: Forrest, acc: Int): Int = {
    if (forrest.isEdge(coordinate)) acc else {
      val newCoordinate = coordinate.move(direction)
      val sideTree = forrest.value(newCoordinate)
      if (treeHeight == sideTree) acc + 1 else if (treeHeight > sideTree) {
        countVisibleTrees(treeHeight, newCoordinate, direction, forrest, acc + 1)
      } else acc + 1
    }
  }

  private def isVisible(threeHeight: Int, coordinate: Coordinate, direction: Direction, forrest: Forrest): Boolean = {
    if (forrest.isEdge(coordinate)) true else {
      val newCoordinate = coordinate.move(direction)
      threeHeight > forrest.value(newCoordinate) && isVisible(threeHeight, newCoordinate, direction, forrest)
    }
  }

  override def part1Answer(): String = {
    forrest.coordinates.map {
      coordinate => Direction.all().exists(isVisible(forrest.value(coordinate), coordinate, _, forrest))
    }.count(_ == true).toString
  }

  override def part2Answer(): String = {
    forrest.coordinates.map {
      coordinate => {
        val height = forrest.value(coordinate)
        val scores = Direction.all().map(countVisibleTrees(height, coordinate, _, forrest, 0))
        scores.filter(_ != 0).product
      }
    }.max.toString
  }

  private case class Forrest(data: Array[Array[Int]]) {
    private val length = data.length - 1
    val coordinates: Seq[Coordinate] = data.indices.flatMap {
      index => List(index).zipAll(data.indices, index, index)
    }.collect {
      case (x, y) => Coordinate(X(x), Y(y))
    }

    def value(coordinate: Coordinate): Int = data(coordinate.y.value)(coordinate.x.value)

    def isEdge(coordinate: Coordinate): Boolean =
      List(0, length).contains(coordinate.x.value) || List(0, length).contains(coordinate.y.value)

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

  object Direction {
    def all(): List[Direction] = List(Right, Left, Top, Bottom)
  }

  sealed trait Direction

  object Right extends Direction

  object Left extends Direction

  object Top extends Direction

  object Bottom extends Direction

}