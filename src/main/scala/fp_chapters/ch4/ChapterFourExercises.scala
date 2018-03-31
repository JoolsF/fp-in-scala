package fp_chapters.ch4

object ChapterFour {

  /**
    * Exercise (4.1) p 54
    * Implement all of the preceding functions on Option. As you implement each function, try to think about
    * what it means and in what situations you’d use it. We’ll explore when to use each of these functions next.
    * Here are a few hints for solving this exercise:
    * It’s fine to use pattern matching, though you should be able to implement all the functions besides map and
    * getOrElse without resorting to pattern matching.   For map and flatMap, the type signature should be enough
    * to determine the implementation.
    *
    * getOrElse returns the result inside the Some case of the Option, or if the Option is None, returns the given
    * default value.
    *
    * orElse returns the first Option if it’s defined; otherwise, it returns the second Option.
    */
  trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))

    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(x) => x
      case None => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f) getOrElse None


    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(x) if f(x) => this
      case None => None
    }

  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]


  /**
    * Exercise (4.2) p 55
    * Implement the variance function in terms of flatMap. If the mean of a sequence is m, the variance is the mean
    * of math.pow(x - m, 2) for each element x in the sequence. See the definition of variance
    * on Wikipedia (http://mng.bz/0Qsr).
    *
    * Work out the Mean
    * Then for each number: subtract the Mean and square the result (the squared difference).
    * Then work out the average of those squared differences. (Why Square?)
    */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(x => Math.pow(x - m, 2)))
    }


  variance(Seq(1, 3, 5, 6, 7, 9, 13, 18, 26))

  /**
    * Exercise (4.3) p 58
    *
    * Write a generic function map2 that combines two Option values using a
    * binary function. If either Option value is None, then the return value is
    * too.
    *
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  /**
    * Write a function sequence that combines a list of Options into one Option
    * containing a list of all the Some values in the original list.
    *
    * If the original list contains None even once, the result of the function should be None;
    * otherwise the result should be Some with a list of all the values.
    *
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(List()))((oa, ob) => map2(oa, ob)(_ :: _))

}
