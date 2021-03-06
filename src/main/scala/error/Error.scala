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

object Option {

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap(a2 => b map (b2 => f(a2, b2)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (h2 => sequence(t) map (h2 :: _))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

}

object Insurance {

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    numberOfSpeedingTickets + (25.0 / age)
  }

  def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Option.Try(age.toInt)
    val optTickets: Option[Int] = Option.Try(numberOfSpeedingTickets.toInt)
    Option.map2(optAge, optTickets)(insuranceRateQuote(_, _))
  }

}

object Sequence {

  def parseInts(a: List[String]): Option[List[Int]] =
    Option.sequence(a map (i => Option.Try(i.toInt)))

}
