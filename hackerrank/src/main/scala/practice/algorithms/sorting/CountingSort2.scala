package practice.algorithms.sorting

import scala.collection.mutable.ListBuffer

class CountingSort2 {

  // Complete the countingSort function below.
  def countingSort(arr: Array[Int]): Array[Int] = {
    val counting = Array.ofDim[Int](100)
    for (i <- arr) {
      counting(i) += 1
    }
    val result = ListBuffer[Int]()
    for (j <- counting.indices) {
      result.appendAll(List.fill(counting(j))(j))
    }
    result.toArray
  }

}
