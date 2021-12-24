object Day5 extends App with AoCPuzzle {
  override def dayNr = 5
  val processedData = getData().map(coordinate => {
    def toCoordinate(coordStr: String) = Point(coordStr.split(",").head.toInt, coordStr.split(",").tail.head.toInt)
    val split = coordinate.split(" -> ")
    Line(toCoordinate(split.head), toCoordinate(split.tail.head))
  })

  override def part1Answer() = {
    val points = processedData.flatMap(_.straightLines())
    val sizeByPoint = count(points)
    sizeByPoint.toString
  }

  override def part2Answer() = {
    val points = processedData.flatMap(_.straighOrDioganal())
    val sizeByPoint = count(points)
    sizeByPoint.toString
  }

  private def count(points: List[Point]) = {
    points.groupBy(identity).view.mapValues(_.length).count(_._2 > 1)
  }

  case class Line(from: Point, to: Point) {
    def straightLines(): List[Point] =
      if (from.x == to.x) {
        val min = Integer.min(from.y, to.y)
        val max = Integer.max(from.y, to.y)
        (min to max).map(y => Point(from.x, y)).toList
      } else if (from.y == to.y) {
        val min = Integer.min(from.x, to.x)
        val max = Integer.max(from.x, to.x)
        (min to max).map(x => Point(x, from.y)).toList
      } else List.empty

    def straighOrDioganal() =
      val lines = straightLines()
      if (lines.nonEmpty) lines else {
        xRange(from, to).zip(yRange(from, to)).map(elem => Point(elem._1, elem._2)).toList
      }

    private def yRange(from: Point, to: Point) = {
      from.y to to.y by (if (from.y > to.y) -1 else 1)
    }

    private def xRange(from: Point, to: Point) = {
      from.x to to.x by (if (from.x > to.x) -1 else 1)
    }
  }

  case class Point(x: Int, y: Int)
}
