package fp_chapters

object ChapterFour {

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
      this map (x => Some(x)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(x) if f(x) => this
      case None => None
    }

  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]


}
