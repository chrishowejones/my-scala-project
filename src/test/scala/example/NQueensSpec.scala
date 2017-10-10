package example

import org.scalatest._

class NQueensSpec extends FlatSpec with Matchers {

  "A Queen " should "be in check if it's on the same row as another queen" in {
    NQueens.inCheck((1, 1), (1, 8)) shouldBe true
  }

  it should "be in check if it's on the same column as another queen" in {
    NQueens.inCheck((1, 1), (3, 1)) shouldBe true
  }

  it should "be in check if it's on the same diagonal as another queen" in {
    NQueens.inCheck((2, 2), (4, 4)) shouldBe true
    NQueens.inCheck((2, 2), (6, 6)) shouldBe true
    NQueens.inCheck((6, 6), (2, 2)) shouldBe true
    NQueens.inCheck((4, 4), (2, 2)) shouldBe true
  }

  it should "not be in check if it's not on the same row, column or diagonal as another queen" in {
    NQueens.inCheck((1, 1), (2, 3)) shouldBe false
    NQueens.inCheck((3, 3), (8, 2)) shouldBe false
  }

  "A Queen" must " be safe if all she is not inCheck from any of the queens in the current list of solutions" in {
    NQueens.isSafe((1, 1), List((3, 5), (8, 3), (2, 7))) shouldBe true
  }

  it should "not be safe is she is in check from any of the queens in the current list of solutions" in {
    NQueens isSafe ((1, 1), List((2, 3), (3, 5), (8, 3), (8, 8))) shouldBe false
    NQueens isSafe ((1, 1), List((2, 3), (3, 5), (8, 3), (1, 8))) shouldBe false
    NQueens isSafe ((1, 1), List((2, 3), (3, 5), (8, 3), (8, 1))) shouldBe false
  }

  "4 Queens" can "be placed on a board as (1,3) (2, 1) (3, 4) (4, 2)" in {
    NQueens.queens(4).map(soln => soln.sortWith((q1, q2) =>
      {
        (q1._1 < q2._1) || (q1._1 == q2._2 && q1._2 < q2._2)
      })).contains(List((1, 3), (2, 1), (3, 4), (4, 2))) shouldBe true
  }

}
