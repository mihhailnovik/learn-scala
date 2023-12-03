package mi.advent.solution

object Day7 extends Day(7) {
  val pattern = "Step (\\w+) must be finished before step (\\w+) can begin.".r
  val data = lines.map {
    case pattern(before, after) => (before, after)
  }
  val dependencies = data.foldLeft(Map[String, List[String]]())((map, pair) => {
    map.updated(pair._2, pair._1 :: map.getOrElse(pair._2, List()))
  }).withDefault(_ => List[String]())

  def sortDependencies(dependencies: Map[String, List[String]]): List[String] = {
    val allElements = dependencies.flatMap(a => a._1 :: a._2).toList
    if (allElements.size == 2) List(allElements.last, allElements.head) else { // special case when only one dependency is defined
      val noDependency = allElements.filter(!dependencies.keySet.contains(_)).distinct.sorted
      if (noDependency.isEmpty) {
        List()
      } else {
        val cleaned = dependencies.mapValues(_.filter(_ != noDependency.head)).filter(_._2.nonEmpty)
        val result = noDependency.head :: sortDependencies(cleaned)
        val resultCleaned = cleaned.mapValues(_.filter(!result.contains(_))).filter(_._2.nonEmpty)
        result ::: sortDependencies(resultCleaned)
      }
    }
  }

  override def part1Solution = sortDependencies(dependencies).mkString("")


  val toSeconds = (a:String) => a.toCharArray.head - 4
  val workers = 5

  def complete(dependencies: Map[String, List[String]], workers:Int, timer: Int) = {
    val allElements = dependencies.flatMap(a => a._1 :: a._2).toList
    val noDependency = allElements.filter(!dependencies.keySet.contains(_)).distinct.sorted.headOption


  }

  override def part2Solution = complete(dependencies, workers, 0)





}