package sandbox

object EqInstances {
  import cats._
  import cats.implicits._

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (date1, date2) =>
      date1.age === date2.age
    }

}
