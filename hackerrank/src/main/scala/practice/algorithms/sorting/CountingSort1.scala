package practice.algorithms.sorting

object CountingSort1 {

  def countingSort(arr: Array[Int]): Array[Int] = {
    val counting = Array.ofDim[Int](100)
    for (i <- arr) {
      counting(i) += 1
    }
    counting
  }

}
