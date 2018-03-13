package fp_chapters

import List.{tail, _}

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

  /**
    * Exercise 3.11 (page 41)
    * Write sum, product, and a function to compute the length of a list using foldLeft
    */

  val sum: List[Int] => Int = foldLeft(_: List[Int], 0: Int)((a, b) => a + b)
  val product: List[Int] => Int = foldLeft(_: List[Int], 1: Int)((a, b) => a * b)

  def length2[A](list: List[A]) = foldLeft(list, 0: Int)((a, b) => a + 1)

  /**
    * Exercise 3.12 (page 41)
    * Write a function that returns the reverse of a list (given List(1,2,3)
    * it returns List(3,2,1)). See if you can write it using a fold.
    */

  def reverse[A](l: List[A]) = List.foldLeft(l, List[A]()) { (a, b) => Cons(b, a) }


  /**
    * Exercise 3.13 (page 41)
    * Hard: Can you write foldLeft in terms of foldRight?
    * How about the other way around? Implementing foldRight via foldLeft is useful because
    * it lets us implement foldRight tail-recursively, which means it works even for large lists
    * without overflowing the stack.
    */


  def foldLeftViaRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, z)((a: A, b: B) => f(b, a))

  def foldRightViaLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b: B, a: A) => f(a, b))

  /**
    * Exercise 3.14 (page 41)
    * Implement append in terms of either foldLeft or foldRight.
    */

  def append[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(l1, l2)((a, b) => Cons(b, a))

  /**
    * Exercise 3.15 (page 41)
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists. Try to use functions
    * we have already defined.
    */

  //  def concatAlternative[A](l1: List[List[A]]): List[A] = l1 match {
  //    case Nil => Nil
  //    case Cons(a, b) => append(a, concat1(b))
  //  }

  def concat[A](l1: List[List[A]]): List[A] =
    foldRight(l1, List[A]())(append)

  /**
    * Exercise 3.16 (page 42)
    * Write a function that transforms a list of integers by adding 1 to each element.
    * (Reminder: this should be a pure function that returns a new List!)
    */
  //fold left would reverse the list here i.e foldLeft(l, List[Int]())((a, b) => Cons(b + 1, a))
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))

  /**
    * Exercise 3.17 (page 42)
    * Write a function that turns each value in a List[Double] into a String.
    * You can use the expression d.toString to convert some d: Double to a String.
    */

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((a, b) => Cons(a.toString, b))

  /**
    * Exercise 3.18 (page 43)
    * Write a function map that generalizes modifying each element in a list while maintaining
    * the structure of the list. Here is its signature:
    * def map[A,B](as: List[A])(f: A => B): List[B]
    */

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((a, b) => Cons(f(a), b))


  /**
    * Exercise 3.19 (page 43)
    * Write a function filter that removes elements from a list unless they satisfy a given
    * predicate. Use it to remove all odd numbers from a List[Int].
    * def filter[A](as: List[A])(f: A => Boolean): List[A]
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]()) { (a, b) =>
      if (f(a)) Cons(a, b) else b
    }

  /**
    * Exercise 3.20 (page 42)
    * Write a function flatMap that works like map except that the function given will return a list instead
    * of a single result, and that list should be inserted into the final resulting list. Here is its signature:
    *
    * def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]
    *
    * For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
    */

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  /**
    * Exercise 3.21 (page 43)
    * Use flatMap to implement filter.
    */

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else List())
  }

  /**
    * Exercise 3.22 (page 43)
    * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
    * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */
  def addListElements(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => l1
    case (_, Nil) => l1
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addListElements(t1, t2))
  }

  /**
    * Exercise 3.23 (page 43)
    * Generalize the function you just wrote so that it’s not specific to integers or addition.
    * Name your generalized function zipWith.
    */

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, _) => l1
    case (_, Nil) => l1
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  /**
    * Exercise 3.24 (page 44)
    * Hard: As an example, implement hasSubsequence for checking whether a List contains another List
    * as a subsequence. For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences,
    * among others.
    * You may have some difficulty finding a concise purely functional implementation that is also efficient.
    */

  def removeLastElement[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, removeLastElement(t))
  }

  //TODO improve solution
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def checkTail(l: List[A]): Boolean = l match {
      case Nil => false
      case l => if (l == sub) return true else checkTail(tail(l))
    }


    def check(l: List[A]): Boolean = l match {
      case Nil => false
      case l => if (checkTail(l)) true else check(removeLastElement(l))
    }

    check(sup)
  }

  // Trees
  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  /**
    * Exercise 3.25 (page 46)
    * Write a function size that counts the number of nodes (leaves and branches) in a tree.
    */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  /**
    * Exercise 3.26 (page 46)
    * Write a function maximum that returns the maximum element in a Tree[Int].
    */

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  /**
    * Exercise 3.27 (page 46)
    * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
    */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) =>
      val lDepth = 1 + depth(l)
      val rDepth = 1 + depth(r)
      if (lDepth > rDepth) lDepth else rDepth
  }

  /**
    * Exercise 3.28 (page 46)
    * Write a function map, analogous to the method of the same name on List
    * that modifies each element in a tree with a given function.
    */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
    * Exercise 3.29 (page 47)
    * Generalize size, maximum, depth, and map, writing a new function fold that
    * abstracts over their similarities. Reimplement them in terms of this more general function.
    * Can you draw an analogy between this fold function and the left and right folds for List?
    */

  def foldTree[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {

    def foldTreeInner(t: Tree[A]): B = t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(foldTreeInner(l), foldTreeInner(r))
    }

    foldTreeInner(tree)
  }

  def fSize[A](t: Tree[A]): Int = foldTree(t)(_ => 1)((l, r) => l + r)

  def fMax(t: Tree[Int]): Int = foldTree(t)(identity)((l, r) => l.max(r))

  def fDepth[A](t: Tree[A]): Int = foldTree(t)(_ => 0)((l, r) => if (l > r) 1 + l else 1 + r)

  def fMap[A, B](t: Tree[A])(f: A => B): Tree[B] = foldTree(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))


}

