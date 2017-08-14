package fp.pure

object PureFunctional extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt: (Int, RNG) = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
          ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int],
          simple(seed2))
      }
    }


    // EXERCISE 1
    def positiveInt(rng: RNG): (Int, RNG) = {
      val (i, ig) = rng.nextInt
      (Math.abs(i - 1), ig)
    }

    //EXERCISE 2: Write a function to generate a Double between 0 and 1,
    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = positiveInt(rng)
      (i.toDouble / Integer.MAX_VALUE.toDouble, r)
    }

    //EXERCISE 3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r) = rng.nextInt
      val (d, r2) = double(r)
      ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), r) = intDouble(rng)
      ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }

    // EXERCISE 4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      count match {
        case a if a < 1 => (List[Int](), rng)
        case a if a >= 1 => {
          val (i, r) = rng.nextInt
          val (i2, r2) = ints(count -1)(r)
          (i :: i2, r2)
        }
      }
    }
    type Rand[+A] = RNG => (A, RNG)
//    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    // EXERCISE 5 Use map to generate an Int between 0 and n, inclusive:
//    def positiveMax(n: Int): Rand[Int] = {
//      map(a => (2, a))()
//    }
  }
  println(RNG.ints(5)(RNG.simple(1)))

}
