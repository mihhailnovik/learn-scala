import Day4.BingoBoard

import scala.annotation.tailrec

object Day4 extends App with AoCPuzzle {
  override def dayNr = 4

  val bingoChallenge = formatData()

  override def part1Answer() = {
    findWinnerScore(bingoChallenge).toString
  }

  override def part2Answer() = {
    findLastWinnerScore(bingoChallenge).toString
  }

  @tailrec
  def findWinnerScore(challenge: BingoChallenge): Int = {
    val roundResult = challenge.nextRound()
    if (roundResult.winnerBoards.nonEmpty)
      roundResult.winnerBoards.head._2
    else {
      findWinnerScore(roundResult)
    }
  }

  @tailrec
  def findLastWinnerScore(challenge: BingoChallenge): Int = {
    val roundResult = challenge.nextRound()
    if (roundResult.inGameBoards.isEmpty && roundResult.winnerBoards.nonEmpty)
      roundResult.winnerBoards.head._2
    else {
      findLastWinnerScore(roundResult)
    }
  }

  def extractAndCleanData(data: List[String]): Seq[Int] = {
    data.tail.filter(_.nonEmpty).flatMap(_.split(" ").filter(_.nonEmpty).map(_.toInt))
  }

  private def formatData(): BingoChallenge = {
    val data = getData()
    val numbers = data.head.split(",").map(_.toInt).toSeq
    val boards = extractAndCleanData(data).sliding(25, 25).map(values => values.map(BingoCell(_, false))).map(BingoBoard.apply).toList
    BingoChallenge(numbers, boards)
  }

  case class BingoChallenge(numbers: Seq[Int], inGameBoards: Seq[BingoBoard], winnerBoards: Seq[(BingoBoard, Int)] = Seq.empty) {
    def nextRound(): BingoChallenge = {
      val nr = numbers.head
      val roundResult = inGameBoards.map(_.update(nr)).groupBy(_.score.isDefined).withDefaultValue(Seq.empty)
      val remainInGame: Seq[BingoBoard] = roundResult(false).map(_.bingoBoard)
      val winners = roundResult(true).map(bbu => (bbu.bingoBoard, bbu.score.get))
      BingoChallenge(numbers.tail, remainInGame, winners)
    }
  }

  case class BingoBoard(cells: Seq[BingoCell]) {
    def update(nr: Int): BingoBoardUpdated = {
      val result = BingoBoard(cells.map {
        cell => if (cell.nr == nr && !cell.crossed) cell.copy(crossed = true) else cell
      })
      BingoBoardUpdated(result, result.winnerScore(nr))
    }

    def winnerScore(number: Int): Option[Int] = {
      val horizontals: Iterator[Seq[BingoCell]] = cells.sliding(5, 5)
      val verticals: Iterable[Seq[BingoCell]] = cells.zipWithIndex.groupBy(_._2 % 5).values.map(_.map(_._1))
      horizontals.find(_.forall(_.crossed)).orElse(verticals.find(_.forall(_.crossed))).map {
        winnerCells => unmarkedNumbersSum * number
      }
    }

    private def unmarkedNumbersSum = cells.filter(!_.crossed).map(_.nr).sum
  }

  case class BingoCell(nr: Int, crossed: Boolean)

  case class BingoBoardUpdated(bingoBoard: BingoBoard, score: Option[Int])

  println(part1Answer())
  println(part2Answer())
}
