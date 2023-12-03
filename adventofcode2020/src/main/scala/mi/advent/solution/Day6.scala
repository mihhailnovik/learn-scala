package mi.advent.solution

import scala.collection.convert.WrapAsJava.`deprecated mapAsJavaMap`

object Day6 extends Day(6) {
  type Cord = (Int, Int)
  type Distance = (String, Int)
  val distance = (a: Cord, b: Cord) => Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)
  val pattern = "(\\d+), (\\d+)".r
  val coordinates = lines.map { case pattern(x, y) => (x.toInt, y.toInt) }
  val (maxX, minX, maxY, minY) = (coordinates.map(_._1).max, coordinates.map(_._1).min, coordinates.map(_._2).max, coordinates.map(_._2).min)
  val allCoordinates = for (x <- minX to maxX; y <- minY to maxY) yield (x, y)

  val result = coordinates.scanLeft(Map[Cord, List[Distance]]()) {
    case (map, point) =>
      allCoordinates.foldLeft(map)((acc, cord) => {
        val dis = distance(point, cord)
        acc.updated(cord, (point.toString, dis) :: acc.getOrDefault(cord, List()))
      })
  }.last

  val notInfiniteCoordinates = coordinates.filter(coord => coord._1 < maxX && coord._1 > minX && coord._2 < maxY && coord._2 > minY)

  override def part1Solution = notInfiniteCoordinates.map(coord => result.count {
    case ((_, _), distances) =>
      val minimum = distances.minBy(_._2)
      minimum._1 == coord.toString && distances.count(_._2 == minimum._2) == 1
  }).max

  override def part2Solution = result.count(_._2.map(_._2).sum < 10000)

  printSolution
}
