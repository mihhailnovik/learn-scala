package sandbox
import cats.implicits._

class SuperAdder {
  def add(items: List[Int]): Int = items match {
    case x::xs =>  x + add(xs)
    case Nil => 0
  }

  def addOpt(items: List[Option[Int]]): Int = {
    add(items.flatten)
  }


}


object SuperAdderMain extends App {
  println(new SuperAdder().addOpt(List(1.some,2.some,3.some,4.some,5.some)))
}
