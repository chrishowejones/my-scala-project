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


  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Variance {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
