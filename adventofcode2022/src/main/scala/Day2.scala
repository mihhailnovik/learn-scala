import java.awt.Choice

class Day2 extends AoCPuzzle {
  override def dayNr: Int = 2

  private val data = getData().map(_.split(" "))

  override def part1Answer(): String = {
    val finalResult = data.map {
      rowArr => GameRound(Choice.fromLetter(rowArr(0)), Choice.fromLetter(rowArr(1)))
    }.map(_.playerScore()).sum
    finalResult.toString
  }

  override def part2Answer(): String = {
    val finalResult = data.map {
      rowArr => GameRound(Choice.fromLetter(rowArr(0)), Choice.fromResult(rowArr(1), Choice.fromLetter(rowArr(0))))
    }.map(_.playerScore()).sum
    finalResult.toString
  }
}


object Choice {
  def fromLetter(letter: String): Choice = letter match
    case "A" | "X" => Rock
    case "B" | "Y" => Paper
    case "C" | "Z" => Scissors
    case _ => Unknown

  def fromResult(result: String, enemy: Choice): Choice = {
    result match
      case "Y" => enemy
      case "X" => enemy.win
      case "Z" => enemy.lose
      case _ => Unknown
  }
}

sealed trait Choice {
  def win: Choice

  def lose: Choice

  def movePoints: Int
}

object Rock extends Choice {
  override def win: Choice = Scissors

  override def lose: Choice = Paper

  override def movePoints: Int = 1
}

object Paper extends Choice {
  override def win: Choice = Rock

  override def lose: Choice = Scissors

  override def movePoints: Int = 2
}

object Scissors extends Choice {
  override def win: Choice = Paper

  override def lose: Choice = Rock

  override def movePoints: Int = 3
}

object Unknown extends Choice {
  override def win: Choice = Unknown

  override def lose: Choice = Unknown

  override def movePoints: Int = 0
}


case class GameRound(enemy: Choice, player: Choice) {
  def playerScore(): Int = {
    val resultPoints = if (player == enemy) 3 else if (player.win == enemy) 6 else 0
    resultPoints + player.movePoints
  }
}
