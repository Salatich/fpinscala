package collections

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(l, r) => max(l) max max(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    //def fold[A, B](t: Tree[A], z: B)(f: (A, B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => l + r + 1)

  def maxViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)((l, r) => l max r)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => (l max r) + 1)

  def leaf[A](a: A): Tree[A] = Leaf(a)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a => leaf(f(a)))((l, r) => Branch(l, r))
  }
}
