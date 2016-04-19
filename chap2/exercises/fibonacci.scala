def fibonacci(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, acc: Int): Int =
    if (n == 0) acc
    if (n == 1 | n == 2) go(n - 1, 1 + acc)
    else go(n - 1, n * acc)
  go(n, 1)
}
