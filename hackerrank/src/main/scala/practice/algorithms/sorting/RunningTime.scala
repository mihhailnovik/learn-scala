package practice.algorithms.sorting

object RunningTime {
  var shifts = 0;

  // Complete the runningTime function below.
  def runningTime(arr: Array[Int]): Int = {
    insertionSort2(arr.toList)
    shifts
  }

  def insertionSort2(list: List[Int]) = {
    var tempList = list

    def insert(index: Int, list: List[Int]): List[Int] = {
      val rightSmaller = index + 1 < list.length && list(index + 1) < list(index)
      val leftBigger = index > 0 && list(index - 1) > list(index)
      if (rightSmaller) {
        val swapped = list.updated(index, list(index + 1)).updated(index + 1, list(index))
        shifts = shifts + 1
        insert(index, swapped)
      } else if (leftBigger) {
        val swapped = list.updated(index, list(index - 1)).updated(index - 1, list(index))
        shifts = shifts + 1
        insert(index - 1, swapped)
      } else list
    }

    for (i <- 0 to tempList.length - 2) {
      tempList = insert(i, tempList)
    }
  }

  def main(args: Array[String]): Unit = {
    print(runningTime(Array[Int](2, 1, 3, 1, 2)))
  }

}
