class Day3 extends AoCPuzzle {
  override def dayNr: Int = 3

  override def active(): Boolean = true

  val data1 = getData().map {
    str => {
     val result =  str.splitAt(str.length / 2)

     val str1 = result._1
     val str2 = result._2

     val common: Option[Char] = str1.find(str2.contains)
     common.map(_.toString).getOrElse("")
    }
  }

  val data2 = getData().sliding(3,3).map {
    str => {
      val s1 = str(0)
      val s2 = str(1)
      val s3 = str(2)
      val common = s1.find(c => s2.contains(c) && s3.contains(c))
      common.map(_.toString).getOrElse("")
    }
  }

  def score(s: String): Int = {
    s.toCharArray.map {
      s => if (s.isUpper) s.toInt - 38 else s.toInt - 96
    }.head
  }

  override def part1Answer(): String = {

    data1.map(score).sum.toString

  }

  override def part2Answer(): String = {
    data2.map(score).sum.toString
  }
}
