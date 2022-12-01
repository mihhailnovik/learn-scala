@main def main: Unit = {

  val days = List(new Day1(), new Day2())

  days.collect {
    day =>
    if (day.active()) {
      s"Day ${day.dayNr}: Answer 1 : ${day.part1Answer()} Answer 2: ${day.part2Answer()}"
    } else {
      ""
    }
  }.foreach(println)
}

