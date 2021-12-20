object Day5 extends App with AoCPuzzle {
  override def dayNr = 5


  val processedData = getData().map(coordinate => {
    def toCoordinate(coordStr: String) = Point(coordStr.split(",").head.toInt, coordStr.split(",").tail.head.toInt)

    val split = coordinate.split(" -> ")
    Line(toCoordinate(split.head), toCoordinate(split.tail.head))
  })

  override def part1Answer() = {
    val points = processedData.flatMap(_.straightLines())
    val sizeByPoint = points.groupBy(identity).view.mapValues(_.length).count(_._2 > 1)
    sizeByPoint.toString
  }

  override def part2Answer() = {
    val points = processedData.flatMap(elems => elems.straighOrDioganal())
    val sizeByPoint = points.groupBy(identity).view.mapValues(_.length).count(_._2 > 1)
    sizeByPoint.toString
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
        (from.x to to.x by (if (from.x > to.x) -1 else 1)).zip(from.y to to.y by (if (from.y > to.y) -1 else 1))
          .map(elem => Point(elem._1, elem._2)).toList
      }
  }

  case class Point(x: Int, y: Int)

  println("processedData = " + part2Answer())
}
