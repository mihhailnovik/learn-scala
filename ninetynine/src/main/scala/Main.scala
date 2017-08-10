import scala.util.Random

/**
  * http://aperiodic.net/phil/scala/s-99/
  */
object Main extends App {

  //  P01
  def last[T](list : List[T]) : T = {
    list match {
      case x::Nil => x
      case _::tail => last(tail)
    }
  }

  //  P02
  def penultimate[T](list : List[T]) : T = {
    list match {
      case x :: y :: Nil => x
      case _::tail => penultimate(tail)
    }
  }

  //  P03
  def nth[A](n : Int, list : List[A]) : A = {
    if (n == 0) return list.head
    nth(n-1, list.tail)
  }

  // P04
  def length[A](list: List[A]) : Int = {
    def length(acc: Int, list:List[A]) : Int  = {
      if (list.isEmpty) return acc
      length(acc + 1, list.tail)
    }
    length(0, list)
  }

  // P05
  def reverse[A](list: List[A]) : List[A] = {
    def reverse[A] (acc : List[A], currList: List[A]) : List[A] = {
      currList match {
        case Nil => acc
        case xs :: _ => reverse(xs :: acc, currList.tail)
      }
    }
    reverse(List(), list)
  }

  // P06
  def isPalindrome[A](list:List[A]) : Boolean = {
      if (length(list) <= 1) return true
      if (length(list) == 2) return list.head == list.last
      if (nth(0, list) == nth(length(list) - 1, list)) return isPalindrome(list.tail.init)
      false
  }

  // P07
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  // P08
  def compress(ls : List[Symbol]): List[Symbol] = {
    def compress(acc : List[Symbol], list:List[Symbol]): List[Symbol] = {
      if (list.isEmpty) return acc
      if (acc.last == list.head) {
        return compress(acc, list.tail)
      }
      compress(acc ::: List(list.head), list.tail)
    }
    compress(List(ls.head), ls.tail)
  }

  // P09
  def pack[A](ls: List[A]) : List[List[A]] = {
    ls.foldRight(List[List[A]]()) {
      (h, r) => {
        if (r.isEmpty || r.head.head != h) List(h) :: r
        else (h :: r.head ) :: r.tail
      }
    }
  }

  //p10
  def encode[A] (list : List[A]): List[(Int, A)] = {
    pack(list).map(l => (l.length, l.head))
  }

  //p11
  def encodeModified[A] (list : List[A]): List[Any] = {
    encode(list).map(l => if (l._1 == 1) l._2 else l)
  }

  // p12
  def decode[A] (list: List[(Int, Symbol)]) : List[Symbol] = {
    list.flatMap(a => List.fill(a._1)(a._2))
  }

  // p13
  def encodeDirect(symbols: List[Symbol]) : List[(Int, Symbol)] = {
    symbols.foldRight(List[(Int, Symbol)]()) {
      (h, acc) => {
        if (acc.isEmpty || acc.head._2 != h) (1, h) :: acc
        else (acc.head._1 + 1, h) :: acc.tail
      }
    }
  }

  //p14
  def duplicate(symbols: List[Symbol]): List[Symbol] = {
    symbols.flatMap(a => List(a,a))
  }

  //p15
  def duplicateN(i: Int, symbols: List[Symbol]): List[Symbol] = {
    symbols.flatMap(List.fill(i)(_))
  }

  //p16
  def drop(i: Int, symbols: List[Symbol]): List[Symbol] = {
    symbols.zipWithIndex.filter(a => (a._2+1) % i != 0).map(_._1)
  }

  //p17
  def split(i: Int, symbols: List[Symbol]): List[List[Symbol]] = {
    def splitAcc(i: Int, buf: List[Symbol], currentList: List[Symbol]): List[List[Symbol]] = {
      if (i == buf.length) List(buf, currentList)
      else splitAcc(i, buf :+ currentList.head, currentList.tail)
    }
    splitAcc(i, List.empty, symbols)
  }

  //p18
  def slice(i: Int, i1: Int, symbols: List[Symbol]): List[Symbol] = {
    split(symbols.length -i1, split(i, symbols).last.reverse).last.reverse
  }

  //p19
  def rotate(rotateStart: Int, list: List[Symbol]): List[Symbol] = {
    split(rotateStart, list).last ::: split(rotateStart, list).head
  }

  //p20
  def removeAt[A](removeAtIndex: Int, symbols: List[A]): (List[A], A) = {
    def removeAt(removeAtIndex: Int, current: Int, head: List[A], tail: List[A]): (List[A], A) = {
      if (removeAtIndex == current) {
        return (tail ::: head.tail, head.head)
      }
      removeAt(removeAtIndex, current + 1, head.tail, tail :+ head.head)
    }
    removeAt(removeAtIndex, 0, symbols, List())
  }

  //p21
  def insertAt(symbol: Symbol, i: Int, symbols: List[Symbol]): List[Symbol] = {
    (i, symbols) match {
      case (0, tail) => symbol :: tail
      case (_, tail) => tail.head :: insertAt(symbol, i - 1, tail.tail)
    }
  }

  //p22
  def range(i1: Int, i2: Int): List[Int] = {
    (i1, i2) match {
      case (a, b) if a == b => a :: Nil
      case (a, b) if a > b => throw new IllegalStateException()
      case (a, b) if a < b => a :: range(a + 1, b)
    }
  }

  //p23
  def randomSelect[A](i: Int, symbols: List[A]): List[A] = {
    if (i > symbols.length) { throw new IllegalAccessException()}
    if (i == 0) { return List()}
    val (rest, e) = removeAt(Random.nextInt(symbols.length), symbols)
    e :: randomSelect(i - 1, rest)
  }

  //p24
  def lotto(i: Int, i1: Int): List[Int] = {
    randomSelect(i, range(0, i1))
  }

  //p25
  def randomPermute(symbols: List[Symbol]): List[Symbol] = {
    randomSelect(symbols.length, symbols)
  }

  //p26
  // think about how to get rid of range...
  def combinations(k: Int, symbols: List[Symbol]): List[List[Symbol]] = {
    k match {
      case 0 => List(List())
      case a if a > symbols.length => throw new IllegalArgumentException()
      case a => range(0, symbols.length - a).map(symbols.drop).flatMap(ls => combinations(a - 1, ls.tail).map(ls.head :: _))
    }
  }

  //p27
  // In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
  // Write a function that generates all the possibilities.
//
//  scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//  res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...

  def group3(strings: List[String]): List[List[List[String]]] = {

  }


  println(group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")))
}
