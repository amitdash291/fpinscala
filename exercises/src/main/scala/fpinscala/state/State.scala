package fpinscala.state

import fpinscala.state.RNG.{Rand, Simple}

import scala.runtime.Nothing$


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, newRng) if n == Int.MinValue => (0, newRng)
    case (n, newRng) if n < 0 => (-n, newRng)
    case (n, newRng) => (n, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, newRng) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), newRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, intRng) = nonNegativeInt(rng)
    val (d, doubleRng) = double(intRng)
    ((i, d), doubleRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), newRng) = intDouble(rng)
    ((d, i), newRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(rng: RNG, count: Int, list: List[Int]): (List[Int], RNG) = {
      count match {
        case 0 => (list, rng)
        case _ => {
          val (n, newRng) = rng.nextInt
          go(newRng, count - 1, n :: list)
        }
      }
    }
    go(rng, count, List())
  }

  def nonNegativeEven(rng: RNG): (Int, RNG) =
    map(nonNegativeInt)(i => i - i % 2)(rng)

  def doubleWithMap(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      val z = (Nil, rng)
      fs.foldRight[(List[A], RNG)](z)((rand, acc) => {
        val (aValue, newRng) = rand(acc._2)
        (acc._1 :+ aValue, newRng)
      })
    }

  def sequenceWithMap2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    //    val z: Rand[List[A]] = rng => (Nil, rng)
    //    fs.foldRight[Rand[List[A]]](z)((rand, acc) => map2(rand, acc)(_ :: _))
    //    fs.foldRight[Rand[List[A]]](unit(Nil))((rand, acc) => map2(rand, acc)(_ :: _))
    fs.foldRight[Rand[List[A]]](unit(Nil))(map2(_, _)(_ :: _))
  }

  def intsSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequenceWithMap2(List.fill(count)(int))(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (aValue, newRng) = f(rng)
    g(aValue)(newRng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
//    val greatestValidInt = ((Int.MaxValue / n) * n) - 1
    flatMap(nonNegativeInt)(x => {
      val mod = x % n
      if ((x + (n-1) - mod) < 0) {
        nonNegativeLessThan(n)
      } else {
        unit(mod)
      }
    })
  }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

object test extends App {
  private val rng = RNG.Simple(100002)
  //  println(RNG.doubleWithMap(rng)._1)
  //  println(RNG.double(rng)._1)
  //  println(RNG.intsSequence(5)(rng)._1.mkString(", "))
  //  println(RNG.map2(RNG.int, RNG.nonNegativeInt)((i, d) => s"$i: $d")(rng)._1)
  //  println(RNG.sequence[Int](List(RNG.int, RNG.int))(rng)._1)
  //  println(RNG.sequenceWithMap2[Int](List(RNG.int, RNG.int))(rng)._1)
  println(RNG.nonNegativeLessThan(10)(rng))
}
