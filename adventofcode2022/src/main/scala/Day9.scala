import scala.collection.immutable.Seq
import scala.collection.mutable

class Day9 extends AoCPuzzle {
  override def dayNr: Int = 9

  override def active(): Boolean = true
  case class X(value: Int)
  case class Y(value: Int)

  case class Knot(var current: Coordinate, history: mutable.HashSet[Coordinate] = mutable.HashSet()) {
    history.add(current)
    def right(saveHistory: Boolean = true): Knot = {
      current = current.right()
      if (saveHistory) {
        history.add(current)
      }
      this
    }
    def left(saveHistory: Boolean = true): Knot = {
      current = current.left()
      if (saveHistory) {
        history.add(current)
      }
      this
    }
    def down(saveHistory: Boolean = true): Knot = {
      current = current.down()
      if (saveHistory) {
        history.add(current)
      }
      this
    }
    def up(saveHistory: Boolean = true): Knot = {
      current = current.up()
      if (saveHistory) {
        history.add(current)
      }
      this
    }
  }

  case class Coordinate(x: X, y: Y) {
    def right(): Coordinate = this.copy(x = X(x.value + 1))
    def left(): Coordinate = this.copy(x = X(x.value - 1))
    def down(): Coordinate = this.copy(y = Y(y.value + 1))
    def up(): Coordinate = this.copy(y = Y(y.value - 1))
  }

  case class Move(direction: String, amount: Int)

  val moves: Seq[Move] = getData().map(_.split(" ")).map(s => Move(s(0), s(1).toInt))
  val field = Field()

  case class Field() {
    val knots: Array[Knot] = Array.fill[Knot](10){
      Knot(Coordinate(X(0), Y(0)))
    }

    def singleMove(move: Move) = {
      var head: Knot = knots.head
      move.direction match {
        case "R" => head.right()
        case "U" => head.up()
        case "L" => head.left()
        case "D" => head.down()
        case s: String =>
          println("ERROR direction = " + s)
      }
     knots.tail.foreach {
        elem => {
          head = follow(elem, head)
        }
      }
    }



    def moveKnot(direction: String, knot: Knot) : Knot = {
      direction match {
        case "R" => {
          knot.right()
        }
        case "U" => {
          knot.up()
        }
        case "L" => {
          knot.left()
        }
        case "D" => {
          knot.down()
        }
        case "RU" | "UR" => {
          knot.right(false).up()
        }
        case "RD" | "DR" => {
          knot.right(false).down()
        }
        case "LU" | "UL" => {
          knot.left(false).up()
        }
        case "LD" | "DL" => {
          knot.left(false).down()
        }
        case z: String => {
          println("  ERROR " + z)
          knot
        }
      }
    }

    def follow(follower: Knot, toFollow: Knot): Knot = {
      // if the same lane and head is more
      if (toFollow.current.x.value == follower.current.x.value) {
        val diff = toFollow.current.y.value - follower.current.y.value
        if (Math.abs(diff) > 1) {
          if (diff > 0) { // T
            moveKnot("D", follower)
          } else {
            moveKnot("U", follower)
          }
        } else {
          follower
        }
      } else if (toFollow.current.y.value == follower.current.y.value) {
        val diff = toFollow.current.x.value - follower.current.x.value
        if (Math.abs(diff) > 1) {
          if (diff > 0) { // T
            moveKnot("R", follower)
          } else {
            moveKnot("L", follower)
          }
        } else {
          follower
        }
      } else {
        val diffX = toFollow.current.x.value - follower.current.x.value
        val diffY = toFollow.current.y.value - follower.current.y.value
        val s = StringBuilder()
        if (Math.abs(diffX) > 1) {
          if (diffX > 1) {
            s.append("R")
          } else {
            s.append("L")
          }
          if (Math.abs(diffY) > 0) {
            if (diffY > 0) { // T
              s.append("D")
            } else {
              s.append("U")
            }
          }
        } else if (Math.abs(diffY) > 1) {
          if (diffY > 1) { // T
            s.append("D")
            //  moveTail("D")
          } else {
            s.append("U")
            // moveTail("U")
          }
          if (Math.abs(diffX) > 0) {
            if (diffX > 0) {
              s.append("R")
              // moveTail("R")
            } else {
              s.append("L")
              // moveTail("L")
            }
          }

        }
        if (s.toString.isEmpty) {
          follower
        } else
          moveKnot(s.toString, follower)
      }
    }

    def move(move: Move) = {
      for (_ <- 1 to move.amount) {
        singleMove(Move(move.direction, 1))
      }
    }

  }

  override def part1Answer(): String = {
    moves.foreach {
      move => field.move(move)
    }
    field.knots.last.history.size.toString
  }

  override def part2Answer(): String = "???"
}
