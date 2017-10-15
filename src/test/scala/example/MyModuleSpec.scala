package example

import org.scalatest._

class MyModuleSpec extends FlatSpec with Matchers {
  "Abs" should " return the same number when that number is positive" in {
    MyModule abs 10 shouldEqual 10
  }

  it should "return the positive version when given a negative number" in {
    MyModule abs -10 shouldEqual 10
  }

  "Factorial" should "return one given one" in {
    MyModule factorial 1 shouldEqual 1
  }

  it should "return 2 when given 2" in {
    MyModule factorial 2 shouldEqual 2
  }

  it should "return 24 when given 4" in {
    MyModule factorial 4 shouldEqual 24
  }

  it should "return 120 when given 5" in {
    MyModule factorial 5 shouldEqual 120
  }

  "Fibonacci" should "return 0 when given 1" in {
    MyModule fibonacci 1 shouldEqual 0
  }

  it should "return 1 when given 2" in {
    MyModule fibonacci 2 shouldEqual 1
  }

  it should "return 1 when given 3" in {
    MyModule fibonacci 3 shouldEqual 1
  }

  it should "return 2 when given 4" in {
    MyModule fibonacci 4 shouldEqual 2
  }

  it should "return 3 when given 5" in {
    MyModule fibonacci 5 shouldEqual 3
  }

  it should "return 5 when given 6" in {
    MyModule fibonacci 6 shouldEqual 5
  }

  it should "return 8 when given 7" in {
    MyModule fibonacci 7 shouldEqual 8
  }

  it should "return 13 when given 8" in {
    MyModule fibonacci 8 shouldEqual 13
  }
}
