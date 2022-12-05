@main def main: Unit = {

  val days = List(new Day1(), new Day2(), new Day3(), new Day4(), new Day5(), new Day6())

  days.collect {
    day =>
      if (day.active()) {
        s"Day ${day.dayNr}: Answer 1 : ${day.part1Answer()} Answer 2: ${day.part2Answer()}\n"
      } else {
        ""
      }
  }.foreach(print)
}

