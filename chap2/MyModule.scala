object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  // exercise 2.1
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int =
      if (n == 0) prev
      else go(n - 1, curr, prev + curr)
    go(n, 0, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n + 1 >= as.length) true
      else if (ordered(as(n), as(n + 1)) == false) false
      else loop(n + 1)
    loop(0)
  }

  // exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(factorial(7))
    println(fibonacci(4))
    println(findFirst(Array(7,8,9), (x: Int) => x == 9))
    println(findFirst(Array('a','c','b'), (x: Char) => x == 'b'))
    println(isSorted(Array(1,2,3), (a: Int, b: Int) => a < b))
    println(isSorted(Array(1,0,3), (a: Int, b: Int) => a < b))
}
