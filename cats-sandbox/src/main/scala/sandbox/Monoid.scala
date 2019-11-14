package sandbox

trait Semigroup[A] {
  def combine(x: A, y: A): A
}
trait Monoid[A] extends Semigroup[A] {
  def empty: A


}
object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid
}


class BooleanMonoid extends Monoid[Boolean] {
  override def empty: Boolean = true

  override def combine(x: Boolean, y: Boolean): Boolean = x && y


}


