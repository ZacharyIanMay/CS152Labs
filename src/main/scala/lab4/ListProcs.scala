package lab4

// pipeline implementations
object pipes {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int = ???
  // sum of sums
  def sos(lists: List[List[Double]]): Double = ???
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int = ???
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean = ???
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean = ???
}

// iterative implementations
object iters {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int = ???
  // sum of sums
  def sos(lists: List[List[Double]]): Double = ???
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int = ???
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean = ???
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean = ???
}

// tail recursive implementations
object tails {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int = ???
  // sum of sums
  def sos(lists: List[List[Double]]): Double = ???
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int = ???
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean = ???
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean = ???
}

// classic recursive implementations (i.e., not tail recursive)
object recur {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int = ???
  // sum of sums
  def sos(lists: List[List[Double]]): Double = ???
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int = ???
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean = ???
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean = ???
}

object ListProcs extends App {

  println("Testing pipelines")
  println("" + pipes.socs(List(1, 2, 3))) // 28
  println("" + pipes.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + pipes.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + pipes.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + pipes.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false

  println("Testing iterations")
  println("" + iters.socs(List(1, 2, 3))) // 28
  println("" + iters.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + iters.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + iters.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + iters.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false

  println("Testing recursions")
  println("" + recur.socs(List(1, 2, 3))) // 28
  println("" + recur.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + recur.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + recur.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + recur.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false

  println("Testing tail-recursions")
  println("" + tails.socs(List(1, 2, 3))) // 28
  println("" + tails.sos(List(List(1, 2, 3), List(4, 5), List(6)))) // 21.0
  println("" + tails.countPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // 2
  println("" + tails.somePass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // true
  println("" + tails.allPass(List(1, 2, 3, 4, 5), (n) => n % 2 == 0)) // false


}