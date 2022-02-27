package lab4

// pipeline implementations
object pipes {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int =
    def tri(a: Int, b: Int): Int = a + b*b*b
    elems.filter(_ % 2 == 1).reduce(tri(_, _))
  // sum of sums
  def sos(lists: List[List[Double]]): Double =
    lists.map(_.reduce(_+_)).reduce(_+_)
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int =
    def helper(v: T): Int = if(test(v)) 1 else 0
    vals.map(helper(_)).reduce(_+_)
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean =
    vals.map(test(_)).reduce(_||_)
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean =
    vals.map(test(_)).reduce(_&&_)
}

// iterative implementations
object iters {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int =
    var result = 0
    for(i <- elems if i%2==1)
      result = result + i*i*i
    result
  // sum of sums
  def sos(lists: List[List[Double]]): Double =
    var result = 0.0
    for(l <- lists)
      for(d <- l) result = result + d
    result
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int =
    var result = 0
    for(i <- vals if test(i)) result = result +1
    result
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean =
    var pass = false
    for(i <- vals if test(i)) pass = true
    pass
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean =
    var pass = true
    for(i <- vals if !test(i)) pass = false
    pass
}

// tail recursive implementations
object tails {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int =
    def helper(count: Int, result: Int): Int =
      if(elems.size <= count) result
      else if(elems(count) % 2 == 1) helper(count+1, result+(elems(count)*elems(count)*elems(count)))
      else helper(count+1, result)
    helper(0, 0)
  // sum of sums
  def sos(lists: List[List[Double]]): Double =
    def helper(count: Int, list: Int, result: Double): Double =
      if(lists.size <= list) result
      else if(lists(list).size <= count)helper(0, list+1, result)
      else helper(count+1, list, result+lists(list)(count))
    helper(0, 0, 0)
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int =
    def helper(count: Int, result: Int): Int =
      if(vals.size <= count) result
      else if(test(vals(count))) helper(count+1, result+1)
      else helper(count+1, result)
    helper(0, 0)
  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean =
    def helper(count: Int): Boolean =
      if(vals.size <= count) false
      else if(test(vals(count))) true
      else helper(count+1)
    helper(0)
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean =
    def helper(count: Int): Boolean =
      if(vals.size <= count) true
      else if(!test(vals(count))) false
      else helper(count+1)
    helper(0)
}

// classic recursive implementations (i.e., not tail recursive)
object recur {
  // = sum of cubes of odds
  def socs(elems: List[Int]): Int =
    if(elems.isEmpty) 0
    else
      val pop = elems(0)
      val list = elems.drop(1)
      if(pop%2==1) pop*pop*pop + socs(list)
      else socs(list)
  // sum of sums
  def sos(lists: List[List[Double]]): Double =
    def helper(list: List[Double]): Double =
      if(list.isEmpty) 0
      else
        val pop = list(0)
        val tmpl = list.drop(1)
        pop + helper(tmpl)
    if(lists.isEmpty) 0
    else
      val pop = lists(0)
      val list = lists.drop(1)
      helper(pop) + sos(list)
  // = # pass
  def countPass[T](vals: List[T], test: T => Boolean): Int =
    if(vals.isEmpty) 0
    else
      val pop = vals(0)
      val list = vals.drop(1)
      if(test(pop)) 1 + countPass(list, test)
      else countPass(list, test)

  // true if at least 1 passes
  def somePass[T](vals: List[T], test: T => Boolean): Boolean =
    if(vals.isEmpty) false
    else
      val pop = vals(0)
      val list = vals.drop(1)
      if(test(pop)) true
      else false || somePass(list, test)
  // true if none fail
  def allPass[T](vals: List[T], test: T => Boolean): Boolean =
    if(vals.isEmpty) true
    else
      val pop = vals(0)
      val list = vals.drop(1)
      if(!test(pop)) false
      else true && allPass(list, test)
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