package collections

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Boolean = {
      if (!ordered(as(prev), as(cur))) {
        false
      } else if (n == 2) {
        true
      } else {
        loop(n - 1, cur, cur - 1)
      }
    }

    loop(as.length, as.length - 1, as.length - 2)
  }

  /*  def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }*/
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => y + 1)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) =>
        //println(s"${x}!")
        f(x, foldRight(xs, z)(f))
    }
    /*@annotation.tailrec
    def go(as: List[A], z: B): B = as match {
      case Nil => z
      case Cons(x, xs) => go(xs, f(x, z))
    }

    go(as, z)*/
  }


  def sum(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productLeft(ns: List[Int]) =
    foldLeft(ns, 1)(_ * _)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((list, elem) => Cons(elem, list))
  }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    //fl (list, ...) + 0
    //fr 0 + (list,...)
    foldLeft(reverse(l), z)((a, b) => f(b, a))
    //foldLeft(reverse(l), z)((b,a) => f(a,b))
    //foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  }

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  /*def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {

  }*/

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Oo")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], elem: A): List[A] = l match {
    case Nil => throw new Exception("hah")
    case Cons(_, t) => Cons(elem, t)
  }

  def append[A](l: List[A], elem: A): List[A] = l match {
    case Nil => Cons(elem, Nil)
    case Cons(h, t) => Cons(elem, Cons(h, t))
  }

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((a, b) => Cons(b, a))

  def concat_mine[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])((l, r) => append(l, r))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((elem, fold) => Cons(elem + 1, fold))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((elem, fold) => Cons(elem.toString, fold))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((elem, fold) => if (f(elem)) Cons(elem, fold) else fold)
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    //foldRight(as, Nil: List[B])((elem, fold) => append(f(elem), fold))
    //map => A => List[B] == List[List[B]]
    concat(map(as)(f))
  }

  //def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
  //List(1,2,3,4)
  //List(3,4) = true


  //def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }


  //foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }


  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => throw new Exception("Oo")
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }

    go(l)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def dropWhile1[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}