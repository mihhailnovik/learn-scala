package mi.advent.solution

import java.text.SimpleDateFormat
import java.util.Date

object Day4 extends Day(4) {

  abstract class Action(val date: Date)

  case class BeginShift(override val date: Date, id: String) extends Action(date)

  case class FallASleep(override val date: Date) extends Action(date)

  case class WakeUp(override val date: Date) extends Action(date)

  // [1518-11-01 23:58]
  private val dateFmt = "yyyy-MM-dd HH:mm"
  val formatter = new SimpleDateFormat(dateFmt)

  val dateRegex = "\\[(.+)\\] (.+)".r
  val (shiftRegex, sleepRegex, wakesUpRegex) = ("Guard #(\\d+) begins shift".r, "falls asleep".r, "wakes up".r)

  object Action {
    def apply(data: String): Action = {
      data match {
        case dateRegex(dateStr, rest) =>
          val date = formatter.parse(dateStr)
          rest match {
            case shiftRegex(id) => BeginShift(date, id)
            case sleepRegex() => FallASleep(date)
            case wakesUpRegex() => WakeUp(date)
          }
      }
    }
  }

  val logRecords = lines.map(Action(_)).sortBy(_.date)

  val (_, _, results) = logRecords.scanLeft(("_", 0, Map[(String, Int), Int]())) {
    case ((guardId, sleepStarted, map), event) =>
      event match {
        case BeginShift(_, id) =>
          (id, sleepStarted, map)
        case FallASleep(d) =>
          (guardId, d.getMinutes, map)
        case WakeUp(d) =>
          val updated = (sleepStarted until d.getMinutes).foldLeft(map)((acc, minute) =>
            acc + ((guardId, minute) -> (acc.getOrElse((guardId, minute), 0) + 1))
          )
          (guardId, sleepStarted, updated)
      }
  }.last

  override def part1Solution = {
    val guard = results.groupBy(_._1._1).mapValues(_.values.sum).maxBy(_._2)._1
    val max = results.filterKeys(_._1 == guard).maxBy(_._2)._1
    Integer.valueOf(max._1) * max._2
  }

  override def part2Solution = {
    val max = results.maxBy(_._2)._1
    Integer.valueOf(max._1) * max._2
  }

  printSolution
}
