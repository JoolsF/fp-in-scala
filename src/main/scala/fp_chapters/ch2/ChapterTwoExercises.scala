package fp_chapters.ch2

import scala.annotation.tailrec

object ChapterTwoExercises extends App {

  /**
    * Exercise 2.2 (page 24)
    * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
    * def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
    */
  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as match {
    case x if (x.length <= 1) => true
    case a =>
      if (ordered(a.head, a.tail.head)) isSorted(as.tail, ordered)
      else false
  }


  /**
    * Exercise 2.3 (page 27)
    * Let’s look at another example, currying,9 which converts a function f of two arguments into a function of
    * one argument that partially applies f.
    * Here again there’s only one implementation that compiles. Write this implementation.
    * def curry[A,B,C](f: (A, B) => C): A => (B => C)
    */


  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def curryAlt[A, B, C](f: (A, B) => C): A => (B => C) =
    a => f(a, _)

  /**
    * Exercise 2.4 (page 27)
    * Implement uncurry, which reverses the transformation of curry. Note that since => associates to the right,
    * A => (B => C) can be written as A => B => C.
    * def uncurry[A,B,C](f: A => B => C): (A, B) => C
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // e.g uncurry(curry((a: Int, b: Int) => a + b))


  /**
    * Implement the higher-order function that composes two functions.
    * def compose[A,B,C](f: B => C, g: A => B): A => C
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}

object ChapterTwo_Notes {

  /**
    * 2.6 Following types to implementations (page 26)
    */
  def partial1a[A, B, C](a: A, f: (A, B) => C): B => C =
    (b) => f(a, b)

  // OR
  def partial1b[A, B, C](a: A, f: (A, B) => C): B => C =
    f(a, _)

  /**
    * 2.6 Composing functions (page 27)
    */

  val double = (a: Int) => a * 2
  val doubleLog = double andThen (Math.log(_))

  (1 to 3).map(doubleLog(_)) // res0: scala.collection.immutable.IndexedSeq[Double] = Vector(0.6931471805599453, 1.3862943611198906, 1.791759469228055)


}