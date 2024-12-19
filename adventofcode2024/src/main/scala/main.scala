
@main def main(): Unit = {
  println("Hello AdventOfCode 2024")

  val days = List(
    new Day1(), new Day2()
  )

  days.collect {
    day =>
      if (day.active()) {
        s"Day ${day.dayNr}: Answer 1 : ${day.part1Answer()} Answer 2: ${day.part2Answer()}\n"
      } else {
        ""
      }
  }.foreach(print)

}
