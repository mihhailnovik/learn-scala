package fp

import scala.annotation.tailrec

object GettingStarted extends App {

  /**
    * EXERCISE 1 (optional): Write a function to get the nth Fibonacci number. The
    * first two Fibonacci numbers are 0 and 1, and the next number is always the sum of
    * the previous two. Your definition should use a local tail-recursive function.4
    */
  def fib(n: Int): Int = {
    @tailrec
    def fibTail(a: Int, b: Int, n: Int): Int = {
      if (n == 0) a
      else fibTail(b, a + b, n - 1)
    }
    fibTail(0, 1, n)
  }

  /**
    * EXERCISE 2: Implement isSorted, which checks whether an Array[A] is
    * sorted according to a given comparison function.
    */
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    !as.sliding(2).exists(a => gt(a(0), a(1)))
  }

  /**
    * EXERCISE 3: Implement partial1 and write down a concrete usage of it
    * This function, partial1, takes a value and a function of two arguments,
    * and returns a function of one argument as its result.
    */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  /**
    * EXERCISE 4:
    * converts a function of N arguments into a function of one argument that returns another
    * function as its result. Here again, there is only one implementation that
    * typechecks.
    */
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  /**
    * EXERCISE 5: Implement uncurry, which reverses the transformation of curry.
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /**
    * EXERCISE 6: Implement the higher-order function that composes two functions.
    */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }


}

