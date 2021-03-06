package lab3

import scala.annotation.tailrec

class Lab3 {
  // problem 1
  def compose[T, S, V](f: T=>V, g: S=>T): S=>V =
    def ret(x: S) = f(g(x))
    ret


  // problem 2
  def id[T](x: T): T = x
  def selfIter[T](f: T => T, n: Int): T => T =
    if(n == 0) id
    else if(n == 1) f
    else compose(f, selfIter(f, n-1))

  // problem 3
  def countPass[T](elems: Array[T], test: T => Boolean): Int =
    @tailrec
    def helper(c: Int, cur: Int): Int =
      if(cur == elems.length) c
      else if(test(elems(cur))) helper(c + 1, cur + 1)
      else helper(c, cur + 1)
    helper(0, 0)


  // problem 5
  def makeIter(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int =
    def ret(n: Int) =
      var t = baseVal
      for(i <- 1 to n) t = combiner(t, i)
      t
    ret

  // problem 6
  def deOptionize[T, S](f: T => Option[S]): T => S =
    def g(x: T): S =
      val t = f(x)
      if(t.isEmpty) throw new Exception("Invalid Input: " + x)
      else t.get
    g

}
