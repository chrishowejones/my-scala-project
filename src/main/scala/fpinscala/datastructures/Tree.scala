package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = {

    def countNodes(t1: Tree[A], count: Int): Int =
      t1 match {
        case Leaf(_) => 1
        case Branch(l, r) => size(l) + size(r) + 1
      }

    countNodes(t, 0)
  }

  def maximum(t: Tree[Int]): Int = {

    def maxNode(ti: Tree[Int], m: Int): Int =
      ti match {
        case Leaf(i) => if (i > m) i else m
        case Branch(l, r) => {
          val ml = maxNode(l, m)
          val mr = maxNode(r, m)
          if (ml > mr) ml else mr
        }
      }

    maxNode(t, 0)
  }


}
