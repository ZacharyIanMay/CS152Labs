package lab4

object tester extends App {
  def numFail[T, S](pairs: List[(T, S)], f: T => S): Int = pairs.filter((T, S) => f(T) != S).map((T, S) => 1).reduce(_ + _)

  val squares = List((0, 0), (2, 4), (3, 9), (4, 16), (5, 25))
  def square(n: Int) = n + n
  println(numFail(squares, square)) // = 3
}
