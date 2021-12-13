import Day3.{Counter, data}

import scala.collection.immutable.{AbstractSeq, LinearSeq}

object Day3 extends App with AoCPuzzle {
  override def dayNr = 3

  val rawData = getData()
  val data: List[List[(Char, Int)]] = rawData.map(string => string.zip(0 until string.length).toList)

  override def part1Answer() = {
    val counters = countBits(data)
    val gammaRate = counters.getMostCommon.mkString
    val epsilonRate = counters.getLeastCommon.mkString
    (Integer.parseInt(gammaRate, 2) * Integer.parseInt(epsilonRate, 2)).toString
  }

  override def part2Answer() = {
    val oxygen = oxygenGeneratorRating(data, 0)
    val co2 = co2ScrubberRating(data, 0)
    val oxygenAnswer = oxygen.map(_.map(_._1)).head.mkString
    val co2Answer = co2.map(_.map(_._1)).head.mkString
    (Integer.parseInt(oxygenAnswer, 2) * Integer.parseInt(co2Answer, 2)).toString
  }

  private def co2ScrubberRating(data: List[List[(Char, Int)]], index: Int): List[List[(Char, Int)]] = {
    val counter = countBits(data)
    val leastCommon = counter.counters(index).leastCommon()
    val filteredData = data.filter(_ (index)._1.toString.toInt == leastCommon)
    if (filteredData.size == 1) {
      filteredData
    } else {
      co2ScrubberRating(filteredData, index + 1)
    }
  }

  private def oxygenGeneratorRating(data: List[List[(Char, Int)]], index: Int): List[List[(Char, Int)]] = {
    val counter = countBits(data)
    val mostCommonFirstBit = counter.counters(index).mostCommon(false)
    val filteredData = data.filter(_ (index)._1.toString.toInt == mostCommonFirstBit)
    if (filteredData.size == 1) {
      filteredData
    } else {
      oxygenGeneratorRating(filteredData, index + 1)
    }
  }


  private def countBits(data: List[List[(Char, Int)]]) = {
    val length = data.head.length
    val initialRegistry = CounterRegistry(List.fill(length)(Counter(0, 0)))
    data.foldLeft(initialRegistry)(updateCounter)
  }

  private def updateCounter(counter: CounterRegistry, data: List[(Char, Int)]): CounterRegistry = {
    data match {
      case x :: xs => updateCounter(counter.update(x._2, x._1.toString.toInt), xs)
      case Nil => counter
    }
  }

  case class CounterRegistry(counters: List[Counter]) {
    def update(index: Int, value: Int) = CounterRegistry(counters.updated(index, counters(index).update(value)))

    def getMostCommon = counters.map(_.mostCommon().toString)

    def getLeastCommon = counters.map(_.leastCommon().toString)
  }

  case class Counter(zeros: Int, ones: Int) {

    def update(value: Int) = if (value == 1) one() else zero()

    def zero(): Counter = this.copy(zeros + 1, ones)

    def one(): Counter = this.copy(zeros, ones + 1)

    def mostCommon(zeroWin: Boolean = true) = if (zeroWin && zeros >= ones) 0 else if (zeros > ones) 0 else 1

    def leastCommon(zeroWin: Boolean = true) = if (mostCommon(!zeroWin) == 0) 1 else 0
  }

}
