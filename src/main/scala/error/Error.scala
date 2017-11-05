package error

trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }


  def orElse[B >: A](ob: => Option[B]): Option[B] = ???
  def filter(f: A => Boolean): Option[A] = ???
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]