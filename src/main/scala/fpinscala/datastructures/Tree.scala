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
        case Leaf(i) => i
        case Branch(l, r) => maximum(l) max maximum(r)
      }

    maxNode(t, 0)
  }

  def depth[A](t: Tree[A]): Int = {

    def maxPath(t1: Tree[A], d: Int): Int =
      t1 match {
        case Leaf(_) => 1
        case Branch(l, r) => (depth(l) max depth(r)) + 1
      }

    maxPath(t, 0)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B =
    t match {
      case Leaf(a) => l(a)
      case Branch(lft, rgt) => b(fold(lft)(l)(b), fold(rgt)(l)(b))
    }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => l + r + 1)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(a => a)((l: Int, r: Int) => l max r)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l: Int, r: Int) => (l + 1) max (r + 1))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((a: A) => Leaf(f(a)): Tree[B])((l: Tree[B], r: Tree[B]) => Branch(l, r))
}
