import scala.collection.mutable
import scala.collection.mutable.Map

class Day8 extends AoCPuzzle {
  override def dayNr: Int = 8

  override def active(): Boolean = false

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

  object Direction {
    def all(): List[Direction] = List(Right, Left, Top, Bottom)
  }
  sealed trait Direction {

  }

  object Right extends Direction

  object Left extends Direction

  object Top extends Direction

  object Bottom extends Direction


  private def countVisibleTrees(treeHeight: Int, coordinate: Coordinate, direction: Direction, forrest: Forrest, acc: Int): Int = {
    if (forrest.isEdge(coordinate)) acc else {
      val newCoordinate = coordinate.move(direction)
      val sideTree = forrest.value(newCoordinate)
      if (treeHeight == sideTree) acc + 1 else if (treeHeight > sideTree) {
        countVisibleTrees(treeHeight, newCoordinate, direction, forrest, (acc + 1))
      } else acc + 1
    }
  }

  override def part1Answer(): String = {
    val forrest = Forrest(input)
    var buffer = mutable.ListBuffer[Int]()
    for (x <- input.indices) {
      for (y <- input.indices) {
        val coordinate = Coordinate(X(x), Y(y))
        val height = forrest.value(coordinate)
        val scores = Direction.all().map(countVisibleTrees(height, coordinate, _, forrest, 0))
        println(s"scores $coordinate= "+scores)
        val scenicScore = scores.filter( _ != 0).product

        buffer = buffer.addOne(scenicScore)
//        if (countVisibleTrees(height, coordinate, List(Right, Left, Top, Bottom), forrest)) {
//          counter += 1
//        } else {
//          println("NOT VISIBLE(" + input(y)(x) + ") " + x + " _ " + y)
//        }
      }
    }
    buffer.max.toString
  }

  override def part2Answer(): String =
    "???"
}
