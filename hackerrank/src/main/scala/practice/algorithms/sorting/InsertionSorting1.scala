package practice.algorithms.sorting

object InsertionSorting1 {

  // Complete the insertionSort1 function below.
  def insertionSort1(n: Int, arr: Array[Int], debug: String => Unit) {
    for (i <- (n - 1) to 1 by -1) {
      val element = arr(i)
      if (arr(i - 1) > element) {
        arr(i) = arr(i - 1)
        debug(arr.toList.mkString(" "))
        arr(i - 1) = element
      } else {
        return
      }
    }
    debug(arr.toList.mkString(" "))
  }
}
