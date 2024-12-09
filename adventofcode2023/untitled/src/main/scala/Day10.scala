class Day10 extends AoCPuzzle {
  override def dayNr: Int = 10

  def parseInput(): Array[Array[Char]] = getData().map(_.toArray).toArray

  def findStart(grid: Array[Array[Char]]): Option[(Int, Int)] = {
    grid.indices.flatMap(i => grid(i).indices.collectFirst {
      case j if grid(i)(j) == 'S' => (i, j)
    }).headOption
  }

  def isValidPos(grid: Array[Array[Char]], x: Int, y: Int): Boolean =
    x >= 0 && y >= 0 && x < grid.length && y < grid(0).length && grid(x)(y) != '.'

  def possibleMoves(grid: Array[Array[Char]], x: Int, y: Int): Set[(Int, Int)] = {
    val pipe = grid(x)(y)
    val moves = pipe match {
      case '|' => Set((x - 1, y), (x + 1, y))
      case '-' => Set((x, y - 1), (x, y + 1))
      case 'L' => Set((x, y - 1), (x - 1, y))
      case 'J' => Set((x, y - 1), (x + 1, y))
      case '7' => Set((x + 1, y), (x, y + 1))
      case 'F' => Set((x - 1, y), (x, y + 1))
      case 'S' => Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
      case _ => Set()
    }
    moves.filter { case (nx, ny) => isValidPos(grid, nx, ny) }
  }

  def bfs(grid: Array[Array[Char]], startX: Int, startY: Int): Int = {
    val visited = collection.mutable.Set[(Int, Int)]()
    val queue = collection.mutable.Queue[((Int, Int), Int)]()
    queue.enqueue(((startX, startY), 0))

    var maxDistance = 0

    while (queue.nonEmpty) {
      val ((x, y), dist) = queue.dequeue()
      if (!visited.contains((x, y))) {
        visited.add((x, y))
        maxDistance = Math.max(maxDistance, dist)

        possibleMoves(grid, x, y).foreach { case (nx, ny) =>
          if (!visited.contains((nx, ny))) {
            queue.enqueue(((nx, ny), dist + 1))
          }
        }
      }
    }

    maxDistance
  }

  override def part1Answer(): String = {
    val grid = parseInput()
    findStart(grid) match {
      case Some((startX, startY)) => bfs(grid, startX, startY).toString
      case None => throw new Exception("Start position not found in input grid")
    }
  }

  override def part2Answer(): String = {
    // Implementation for part 2 (if applicable)
    ""
  }

  override def active(): Boolean = true
}

