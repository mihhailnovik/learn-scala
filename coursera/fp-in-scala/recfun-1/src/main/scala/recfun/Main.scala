package recfun

import scala.annotation.tailrec
import scala.collection.mutable.Stack

object Main {
  def main(args: Array[String]) {
    print(countChange(300,List(5,10,20,50,100,200,500)))
  }

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) return 1
    pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceAcc(chars: List[Char], acc: Stack[Int]): Boolean = {
      if (chars.isEmpty) {
        return acc.isEmpty
      }
      val head = chars.head
      if (')' == head) {
        if (acc.isEmpty) {
          return false
        }
        val lastElement = acc.pop()
        if (lastElement != '(') {
          return false
        }
      }
      if ('(' == head) {
        acc.push(head)
      }
      balanceAcc(chars.tail, acc)
    }
    balanceAcc(chars, new scala.collection.mutable.Stack())
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeAcc(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money - coins.head == 0) 1
      else if (money - coins.head < 0) 0
      else countChangeAcc(money - coins.head, coins) + countChangeAcc(money, coins.tail)
    }
    countChangeAcc(money, coins.sorted(Ordering.Int))
  }
//    print(countChange(300, List(5, 10, 20, 50, 100, 200, 500)))
}
