package example

import org.scalatest._

class ShoppingCartSpec extends FlatSpec with Matchers {

  "A ShoppingCart checkout" should "have a value of 60 pence when it contains an apple" in {
    ShoppingCart.checkout(List("apple")) shouldEqual 0.60
  }

  it should "have a value of 25 pence when it contains an orange" in {
    ShoppingCart.checkout(List("orange")) shouldEqual 0.25
  }

  it should "have a value of 120 pence when it contains two apples" in {
    ShoppingCart.checkout("apple" :: "apple" :: Nil) shouldEqual 1.20
  }

  it should "have a value of 50 pence when it contains two oranges" in {
    ShoppingCart.checkout("orange" :: "orange" :: Nil) shouldEqual 0.50
  }

  it should "have a value of 205 pence when it contains 3 apples and an orange" in {
    ShoppingCart.checkout("apple" :: "apple" :: "orange" :: "apple" :: Nil) shouldEqual 2.05
  }

  it should "have a value of 0 pence when it contains null" in {
    ShoppingCart.checkout(List(null, null)) shouldEqual 0.0
  }

  it should "have a value of 60 pence when it contains apple and nulls" in {
    ShoppingCart.checkout(List(null, "apple", null)) shouldEqual 0.60
  }

  it should "throw an illegal state exception when it contains grapes" in {
    the[IllegalStateException] thrownBy
      ShoppingCart.checkout(List("grapes")) should have message "Illegal item added"
  }

  it should "have a value of 0 pence when an empty list is checked out" in {
    ShoppingCart.checkout(List()) shouldEqual 0.0
  }

  it should "have a value of 0 pence when a nil is checked out" in {
    ShoppingCart.checkout(Nil) shouldEqual 0.0
  }

  // same tests for checkoutWithTry

    "A ShoppingCart checkoutWithTry" should "have a value of 60 pence when it contains an apple" in {
    ShoppingCart.checkoutWithTry(List("apple")) shouldEqual 0.60
  }

  it should "have a value of 25 pence when it contains an orange" in {
    ShoppingCart.checkoutWithTry(List("orange")) shouldEqual 0.25
  }

  it should "have a value of 120 pence when it contains two apples" in {
    ShoppingCart.checkoutWithTry("apple" :: "apple" :: Nil) shouldEqual 1.20
  }

  it should "have a value of 50 pence when it contains two oranges" in {
    ShoppingCart.checkoutWithTry("orange" :: "orange" :: Nil) shouldEqual 0.50
  }

  it should "have a value of 205 pence when it contains 3 apples and an orange" in {
    ShoppingCart.checkoutWithTry("apple" :: "apple" :: "orange" :: "apple" :: Nil) shouldEqual 2.05
  }

  it should "have a value of 0 pence when it contains null" in {
    ShoppingCart.checkoutWithTry(List(null, null)) shouldEqual 0.0
  }

  it should "have a value of 60 pence when it contains apple and nulls" in {
    ShoppingCart.checkoutWithTry(List(null, "apple", null)) shouldEqual 0.60
  }

  it should "throw an illegal state exception when it contains grapes" in {
    the[IllegalStateException] thrownBy
      ShoppingCart.checkoutWithTry(List("grapes")) should have message "Illegal item added"
  }

  it should "have a value of 0 pence when an empty list is checked out" in {
    ShoppingCart.checkoutWithTry(List()) shouldEqual 0.0
  }

  it should "have a value of 0 pence when a nil is checked out" in {
    ShoppingCart.checkoutWithTry(Nil) shouldEqual 0.0
  }


}
