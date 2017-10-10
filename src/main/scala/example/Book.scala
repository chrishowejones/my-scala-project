package example

import scala.collection._
import scala.collection.mutable.Buffer
import scala.language.postfixOps

case class Book(title: String, authors: String*)

object Books{

  val books: List[Book] =
  List(
    Book(
      "Structure and the Interpretation of Computer Programs",
      "Abelson, Harold", "Sussman, Gerald J."
    ),
    Book(
      "Principles of Compiler Design",
      "Aho, Alfred", "Ullman, Jeffery"
    ),
    Book(
      "Programming in Modula-2",
      "Wirth, Niklaus"
    ),
    Book(
      "Elements of ML Programming",
      "Ullman, Jeffery"
    ),
    Book(
      "The Java Language Specification",
      "Gosling, James", "Joy, Bill", "Steele, Guy", "Bracha, Gilad"
    )
  )

  for (b <- Books.books; a <- b.authors
    if a startsWith "Ullman")
  yield b.title

  for (b <- Books.books; if (b.title indexOf "Program") >= 0)
  yield b.title

  val authorsMoreThanOneBook =
    for (b1 <- Books.books; b2 <- Books.books if b1 != b2;
    a1 <- b1.authors; a2 <- b2.authors if a1 == a2)
  yield a1

  def removeDuplicates[A] (xs: List[A]): List[A] = {
    if (xs.isEmpty) xs
    else
      xs.head :: removeDuplicates(
        xs.tail filter (x => x != xs.head)
      )
  }

  removeDuplicates(authorsMoreThanOneBook)

  // translate authorsMoreThanOneBook to use map, flatMap, withFilter

  val authorsMoreThanOneBook2 =
    Books.books flatMap(b1 =>
      Books.books withFilter(b2 => b1 != b2) flatMap(b2 =>
        b1.authors flatMap(a1 =>
          b2.authors withFilter(a2 => a1 == a2) map (a2 => a1))))

  println(authorsMoreThanOneBook2)

  println(removeDuplicates(authorsMoreThanOneBook2))

}

object Color extends Enumeration {
  val Red = Value
  val Blue = Value
  val Green = Value
}


object TryStuff {

  val x, y, z = 1

  val a, b, c = 2

  Traversable(1, 2, 3)

  Iterable("x", "y", "z")

  Map("x" -> 24, "y" -> 25, "z" -> 26)

  Set(Color.Red, Color.Green, Color.Blue)

  SortedSet("hello", "world")

  Buffer(x, y, z)

  IndexedSeq(1.0, 2.0)

  LinearSeq(a, b, c)

  List(1, 2, 3) map (_ + 1)

  Set(1, 2, 3) map (_ * 2)

  List(1, 2, 3) foreach(println(_))

  List(1, 2, 3) sum

  List(1, 2, 3) product

  1 to 11 sum

  1 to 11 product

}
