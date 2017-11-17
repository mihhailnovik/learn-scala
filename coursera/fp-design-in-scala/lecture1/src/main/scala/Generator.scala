import Main.Generator

object Main extends App {

  trait Generator[+T] {
    self =>
    def generate: T
    def map[S](f : T => S) : Generator[S] = new Generator[S] {
      override def generate = f(self.generate)
    }

    def flatMap[S] (f : T => Generator[S]) : Generator[S] = new Generator[S] {
      override def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {

    override def generate: Int = {
      new java.util.Random().nextInt()
    }

  }

  val booleans = for (x <- integers) yield x > 0


trait Pree

case class Inner(lefr:Pree, righr:Pree) extends Pree
case class leaf(x: Int) extends Pree

def prees : Generator[Pree]=
  for {
      x <- booleans
      resul <- if (x) leaf(integers.generate) else Inner(prees.generate, prees.generate)
    } yield resul
}