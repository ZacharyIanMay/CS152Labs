package lab2

import scala.annotation.tailrec

class Base {
  def inc(x: BigInt): BigInt = x + 1
  def dec(x: BigInt): BigInt = x - 1
  def isZero(x: BigInt): Boolean = x == 0
}

class Hyper extends Base {

  def add(n: BigInt, m: BigInt): BigInt =
    if(isZero(m)) n
    else inc(add(n,dec(m)))

  def mul(n: BigInt, m: BigInt): BigInt =
    if(isZero(m)) 0
    else add(mul(n, dec(m)), n)

  def exp(n: BigInt): BigInt =
    if(isZero(n)) 1
    else mul(exp(dec(n)), 2)

  def hyperExp(n: BigInt): BigInt =
    if(isZero(n)) 2
    else exp(hyperExp(dec(n)))

  // etc.
}

class TailHyper extends Base {

  final def add(n: BigInt, m: BigInt): BigInt =
    if(isZero(m)) n
    else add(inc(n), dec(m))

  def mul(n: BigInt, m: BigInt): BigInt =
    if(isZero(dec(m))) n
    else mul(add(n,n), dec(m))

  def exp(n: BigInt): BigInt =
    def helper(v: BigInt, c: BigInt): BigInt =
      if(n <= inc(c)) v
      else helper(mul(v,2), inc(c))
    helper(2, 0)

  def hyperExp(n: BigInt): BigInt =
    def helper(v: BigInt, c:BigInt): BigInt =
      if(n <= c) v
      else helper(exp(v), inc(c))
    helper(2,0)

  // etc.
}

object HyperTest extends TailHyper with App {

  println(add(1,1))
  println(mul(1,1))
  println(mul(2,2))
  println("exp(10) = " + exp(10))           // 1024
  println("hyperExp(2) = " + hyperExp(2))   // 16
  println("hyperExp(3) = " + hyperExp(3))   // 65536
  println("hyperExp(4) = " + hyperExp(4))   // still waiting

}

