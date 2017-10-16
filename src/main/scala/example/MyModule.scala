package example

// A comment
/* Another comment */
/** A documentation comment */
object MyModule {

  def abs(n: Int): Int = {
    require(n > Int.MinValue)
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int =
      if (n <= 1) a
      else go(n - 1, b, a + b)

    go(n, 0, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A] (as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n+1 >= as.length) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = a => (b => (f(a,b)))

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => (f(a)(b))

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatResult("abs value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }

}
