package tailrec

object Math {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(acc: Int, prev: Int, cur: Int): Int = {
      if (acc == 0) {
        prev
      } else {
        go(acc - 1, cur, prev + cur)
      }
    }

    go(n, 0, 1)
  }
}
