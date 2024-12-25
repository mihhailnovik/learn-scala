import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class Day4 extends AoCPuzzle :

  override def active(): Boolean = true

  override def dayNr: Int = 4

  def part1Answer(): String = {
    val data: Seq[String] = getData
    val dataArray: Array[Array[Char]] = data.map(_.toCharArray).toArray
    val horizontals = data ++ data.map(_.reverse)
    val verticals = {
      val raw = dataArray(0).indices.map(col => dataArray.map(_(col)).mkString)
      raw ++ raw.map(_.reverse)
    }

    def allDiagonals(data: Array[Array[Char]], dx: Int, dy: Int): Seq[String] = {
      val rows = data.length
      val cols = data(0).length
      @tailrec
      def collectDiag(x: Int, y: Int, acc: List[Char] = Nil): List[Char] = {
        if (x < 0 || x >= rows || y < 0 || y >= cols) acc
        else collectDiag(x + dx, y + dy, acc :+ data(x)(y))
      }
      val starts = (0 until rows).map(r => (r, 0)) ++ (1 until cols).map(c => (if (dx > 0) 0 else rows - 1, c))
      starts.map { case (r, c) => collectDiag(r, c) }.filter(_.length > 3).map(_.mkString)
    }
    val diagDownRight = allDiagonals(dataArray, dx = 1, dy = 1)
    val diagDownRightAll = diagDownRight ++ diagDownRight.map(_.reverse)
    val diagUpRight = allDiagonals(dataArray, dx = -1, dy = 1)
    val diagUpRightAll = diagUpRight ++ diagUpRight.map(_.reverse)
    val totalList = horizontals ++ verticals ++ diagDownRightAll ++ diagUpRightAll
    totalList.map(str => str.sliding(4).count(_ == "XMAS")).sum.toString
  }

  override def part2Answer(): String = {
    val arrData = getData.map(_.toCharArray).toArray
    val size = arrData.length

    val pairs: Seq[(Int, Int)] =
      (0 to size - 3).flatMap { i =>
        (0 to size - 3).map { j =>
          (i, j)
        }
      }

    pairs.count(cord => isXmas(slide(cord._1, cord._2, arrData))).toString
  }


  private def isXmas(arr: Array[Array[Char]]): Boolean =
    arr.length == 3 && arr.forall(_.length == 3) && {
      val diag1 = Array(arr(0)(0), arr(1)(1), arr(2)(2)).mkString
      val diag2 = Array(arr(0)(2), arr(1)(1), arr(2)(0)).mkString
      List(diag1, diag1.reverse).contains("MAS") && List(diag2, diag2.reverse).contains("MAS")
    }

  private def slide(x: Int, y: Int, arr: Array[Array[Char]]): Array[Array[Char]] =
    Array(
      Array(arr(x)(y), arr(x)(y + 1), arr(x)(y + 2)),
      Array(arr(x + 1)(y), arr(x + 1)(y + 1), arr(x + 1)(y + 2)),
      Array(arr(x + 2)(y), arr(x + 2)(y + 1), arr(x + 2)(y + 2))
    )