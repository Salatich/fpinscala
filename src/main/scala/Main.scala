import collections.{Branch, Leaf, Tree}

object Main extends App {

  override def main(args: Array[String]): Unit = {
    val b = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    //println(Tree.size(b))
    // println(Tree.sizeViaFold(b))
    //println(Tree.max(b))
    // println(Tree.depth(b))
    // println(Tree.map(b)(_ + 1))
    println(Tree.fold(b)(Leaf(_): Tree[Int])(Branch(_, _)))
  }

}

//foldLeft (List[Int], Int)
//foldRight(Int,List[Int])
//  println(List.flatMap(l)(i => List(i, i)))
//println(List.filter(l)(_ % 2 == 0))
// println(List.filterViaFlatMap(l)(_ % 2 == 0))
/* println(List.foldLeft(l, Nil: List[Int])((a, b) => f(b, a)))
 println(List.foldLeftViaFoldRight(l, Nil: List[Int])((a, b) => f(b, a)))

 println(List.foldRight(l, Nil: List[Int])((a, b) => f(a, b)))
 println(List.foldRightViaFoldLeft(l, Nil: List[Int])((a, b) => f(a, b)))
 println(List.foldRightViaFoldLeft_1(l, Nil: List[Int])((a, b) => f(a, b)))*/
/* println(List.appendViaFoldLeft(l, r))
 println(List.appendViaFoldRight(l, r))
 println(List.concat(Cons(1, Nil)))*/
