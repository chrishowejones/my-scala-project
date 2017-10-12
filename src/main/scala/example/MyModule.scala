package example

// A comment
/* Another comment */
/** A documentation comment */
object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }

    go(n, 1)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n:Int, a: Int, b:Int): Int =
      if (n <= 1) a
      else go(n-1, b, a + b)

    go(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("abs value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }

}
