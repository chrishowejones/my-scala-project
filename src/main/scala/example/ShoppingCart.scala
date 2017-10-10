package example

import scala.util.Try

class ShoppingCart(priceMatrix: Map[String, () => Double]) {

  def checkout(items: List[String]): Double = {
    def price(total: Double, item: String) = {
      priceMatrix.get(item) match {
        case priceCalc: Some[() => Double] => total + priceCalc.get.apply()
        case None => throw new IllegalStateException("Illegal item added")
      }
    }

    items.filter(_ != null)
      .map(_.toLowerCase)
      .foldLeft(0.0)((total, item) =>
        price(total, item))
  }

  def checkoutWithTry(items: List[String]): Double = {
    def price(total: Double, item: String) = {
      val priceCalc = Try(priceMatrix(item))
        .getOrElse { throw new IllegalStateException("Illegal item added") }
      total + priceCalc()
    }

    items.filter(_ != null)
      .map(_.toLowerCase)
      .foldLeft(0.0) ((total, item) =>
      price(total, item))
  }

}

object ShoppingCart {

  val defaultPriceMatrix =
    Map(("apple", () => 0.60),
      ("orange", () => 0.25),
      ("plum", () => 0.75))

  def checkout(items: List[String]): Double = new ShoppingCart(defaultPriceMatrix).checkout(items)

  def checkoutWithTry(items: List[String]): Double = new ShoppingCart(defaultPriceMatrix).checkout(items)
}
