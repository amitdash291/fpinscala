package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, concatenate(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def printElements[A](l: List[A]): Unit =
    l match {
      case Nil => println()
      case Cons(h, t) => {
        print(h)
        if (t != Nil) print(", ")
        printElements(t)
      }
    }


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) => Cons(h, tail)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) => {
        if (n > 1) drop(tail, n - 1)
        else tail
      }
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) => {
        if (f(head)) dropWhile(tail, f)
        else l
      }
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Cannot remove elements from an empty list")
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

  def optimizedProduct(ns: List[Int]): Int = {
    foldRight(ns, 1)((x, y) => if (x != 0) x * y else 0)
  }


  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x, y) => y + 1)
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumWithFoldLeft(l: List[Int]): Int = {
    foldLeft(l, 0)((a, b) => a + b)
  }

  def productWithFoldLeft(l: List[Int]): Int = {
    foldLeft(l, 1)((a, b) => a * b)
  }

  def lengthWithFoldLeft[A](l: List[A]): Int = {
    foldLeft(l, 0)((x, y) => x + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((b, a) => Cons(a, b))
  }

  //def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B
  // as --> list
  // z --> frZero (B => B)
  // f --> (A, (B => B)) returns (B => B)
  // Make sure that flFunc executes first and then frFunc executes
  // Lastly call the function returned by fold right with acc as input
  def foldLeftUsingFoldRight[A, B](list: List[A], acc: B)(flFunc: (B, A) => B): B = {
    foldRight(list, (frZero: B) => frZero)((a, frFunc) => {
      b => {
        frFunc(flFunc(b, a))
      }
    })(acc)
  }

  def appendElement[A](l: List[A], n: A): List[A] = {
    append(l, Cons(n, Nil))
  }

  def concatenate[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_, _))
  }

  def appendElementWithFoldRight[A](l: List[A], n: A): List[A] = {
    concatenate(l, Cons(n, Nil))
  }

  def transformByAddingOne(l: List[Int]) : List[Int] =
  l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, transformByAddingOne(t))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  //  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
  //  l match {
  //    case Nil => Nil
  //    case Cons(h, t) => Cons(f(h), map(t)(f))
  //  }
}

object Test extends App {
  val testList = List[Int](1, 2, 3, 4);
  //List.printElements(List.drop(testList, 3))
  //List.printElements(List.dropWhile(testList, (x: Int) => (x < 3)))
  //List.printElements(List.init(testList))
  //println(foldRight(testList, 1) (_ * _))
  //println(List.length(testList))
  //println(List.sumWithFoldLeft(testList))
  //println(List.productWithFoldLeft(testList))
  //println(List.lengthWithFoldLeft(testList))
  //List.printElements((List.reverse(testList)))
  //println(List.foldLeftUsingFoldRight(testList, 0)((a, b) => a + b))
  List.printElements(List.appendElement(testList, 5))
  //List.printElements(List.map(List[Int](1,2,3))(i => i + 1))
}
