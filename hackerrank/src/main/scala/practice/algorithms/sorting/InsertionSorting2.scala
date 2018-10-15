package practice.algorithms.sorting

object InsertionSorting2 {

  /**
    * Sample Input
    *
    * 6
    * 1 4 3 5 6 2
    * Sample Output
    *
    * 1 4 3 5 6 2
    * 1 3 4 5 6 2
    * 1 3 4 5 6 2
    * 1 3 4 5 6 2
    * 1 2 3 4 5 6
    *
    */

  // Complete the insertionSort2 function below.
  def insertionSort2(n: Int, arr: Array[Int]) {
    def insertionSort2(list: List[Int]) = {
      var tempList = list

      def insert(index: Int, list: List[Int]): List[Int] = {
        val rightSmaller = index + 1 < list.length && list(index + 1) < list(index)
        val leftBigger = index > 0 && list(index - 1) > list(index)
        if (rightSmaller) {
          val swapped = list.updated(index, list(index + 1)).updated(index + 1, list(index))
          insert(index, swapped)
        } else if (leftBigger) {
          val swapped = list.updated(index, list(index - 1)).updated(index - 1, list(index))
          insert(index - 1, swapped)
        } else list
      }

      for (i <- 0 to tempList.length - 2) {
        tempList = insert(i, tempList)
        println(tempList.mkString(" "))
      }
    }

    insertionSort2(arr.toList)
  }

  def main(args: Array[String]): Unit = {
    insertionSort2(6, Array[Int](1, 4, 3, 5, 6, 2))
  }
}
