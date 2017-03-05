package fp_chapters

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 3.2 (page 35)
    * Implement the function tail for removing the first element of a List. Note that the function takes constant time.
    * What are different choices you could make in your implementation if the List is Nil?
    */

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(hd, tl) => tl
  }

  /**
    * Exercise 3.3 (page 36)
    * Using the same idea, implement the function setHead for replacing the first element of a List with a different
    * value.
    */

  def setHead[A](hd: A, list: List[A]) = list match {
    case Nil => List(hd)
    case Cons(hdOld, tl) => Cons(hd, tl)
  }

  /**
    * Exercise 3.4 (page 36)
    * Generalize tail to the function drop, which removes the first n elements from a list. Note that this function
    * takes time proportional only to the number of elements being dropped—we don’t need to make a copy of the entire
    * List.
    * def drop[A](l: List[A], n: Int): List[A]
    */
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case a if a <= 0 => l
    case b => drop(List.tail(l), n - 1)

  }

  /**
    * Exercise 3.5 (page 36)
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
    * def dropWhile[A](l: List[A], f: A => Boolean): List[A]
    */
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, b) => if (f(a)) dropWhile(b, f) else l
  }

  /**
    * Exercise 3.6 (page 37)
    * Implement a function, init, that returns a List consisting of all but the last element of a List.
    * So, given List(1,2,3,4), init will return List(1,2,3).
    * Why can’t this function be implemented in constant time like tail?
    * def init[A](l: List[A]): List[A]
    */

  def init[A](l: List[A]): List[A] = l match {
    case Cons(a, Nil) => Nil
    case Nil => Nil //in case empty list is passed in
    case Cons(hd, tl) => Cons(hd, init(tl))
  }


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }


  def product(ns: List[Double]) =
    foldRight(ns, 1.0) { (e, acc) =>
      e * acc
    }

  /**
    * Exercise 3.7 (page 40)
    * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters
    * a 0.0? Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
    */
  // Not possible since to stop further processing on list, need to match on Nil case in foldRight
  // This means that list has to be traveresed to end before f can be eval'd
  //   def productShortCircuit(ns: List[Double]) =
  //     foldRight(ns, 1.0) { (e, acc) =>
  //       if(e == 0) ???
  //       else ???
  //    }

  /**
    * Exercise 3.8 (page 40)
    * See what happens when you pass Nil and Cons themselves to foldRight, like this: foldRight(List(1,2,3),
    * Nil:List[Int])(Cons(_,_)).10 What do you think this says about the relationship between foldRight and
    * the data constructors of List?
    */
  //  We get back the original list! Why is that? As we mentioned earlier, one way of thinking about what `foldRight` "does" is it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input list.
  //  foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
  //  Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
  //  Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
  //  Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
  //  Cons(1, Cons(2, Cons(3, Nil)))

  /**
    * Exercise 3.9 (page 40)
    * Compute the length of a list using foldRight.
    * def length[A](as: List[A]): Int
    */

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, b) => b + 1)

  /**
    * Exercise 3.10 (page 40)
    * Our implementation of foldRight is not tail-recursive and will result in a StackOverflow Error for large
    * lists (we say it’s not stack-safe). Convince yourself that this is the case, and then write another general
    * list-recursion function, foldLeft, that is tail-recursive, using the techniques we discussed in the previous
    * chapter
    * def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B
    */

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(hd, tl) => foldLeft(tl, f(z, hd))(f)
  }
}


object ChapterThree extends App {
  val x: List[Int] = Cons(1, Cons(2, Cons(3, Cons(6, Cons(10, Nil)))))
  val y: List[Double] = Cons(1, Cons(2, Cons(3, Cons(6, Cons(10, Nil)))))
  val pred: Int => Boolean = a => a < 4



}

