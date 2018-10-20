package practice.algorithms.sorting

object QuickSortPartition {

  // Complete the quickSort function below.
  def quickSort(arr: Array[Int]): Array[Int] = {
    val pivot = arr(0)
    var resultRight = List[Int]()
    var resultLeft = List[Int]()
    for (i <- 1 until arr.length) {
      if (arr(i) < pivot) {
        resultLeft = resultLeft :+ arr(i)
      } else {
        resultRight = resultRight :+ arr(i)
      }
    }
    ((resultLeft :+ pivot) ::: resultRight).toArray
  }

  // Complete the quickSort function below.
  def quickSortRec(arr: Array[Int]): Array[Int] = {
    def filter(arr: Array[Int], f: Int => Boolean): Array[Int] = {
      def filterAcc(arr: Array[Int], f: Int => Boolean, acc: Array[Int]): Array[Int] = {
        if (arr.length == 0) acc
        else if (f(arr.head)) filterAcc(arr.tail, f, acc :+ arr.head)
        else filterAcc(arr.tail, f, acc)
      }
      filterAcc(arr, f, Array[Int]())
    }
    (filter(arr.tail, i => i < arr(0)) :+ arr(0)) ++ filter(arr.tail, i => i > arr(0))
  }

  def main(args: Array[String]): Unit = {
    println(quickSortRec(Array[Int](4, 5, 3, 7, 2)).mkString(" "))
    // output 3 2 4 5 7
  }
}
