package sandbox

trait Printable[A] {
  def format(o:A):String
}

object Printable {
  def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)
  def print[A](input: A)(implicit p: Printable[A]): Unit = println(format(input))
}

object  PrintableInstances {

  implicit val stringPrintable = new Printable[String] {
    def format(input: String) = input
  }
  implicit val intPrintable = new Printable[Int] {
    def format(input: Int) = input.toString
  }

  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat) = {
      s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
    }
  }
}

object  PrintableSyntax {
  implicit class PrintableOps[A](o:A) {
    def format(implicit printable: Printable[A]) = {
      printable.format(o)
    }
    def print(implicit printable: Printable[A]) = {
      println(printable.format(o))
    }
  }
}

final case class Cat(name: String, age: Int, color: String)
