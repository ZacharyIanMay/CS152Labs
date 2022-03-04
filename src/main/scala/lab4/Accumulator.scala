package lab4

object Accumulator extends App
{
  def accum(program: List[Int => Int]): Int =
    var register = 1
    register = 0
    for(i <- program) register = i(register)
    register

  def add(v: Int): Int => Int =
    (reg: Int) => reg + v

  def mul(v: Int): Int => Int =
    (reg: Int) => reg * v

  def clear(v: Int): Int =
    0

  def display(v: Int): Int =
    println("Register = " + v)
    v

  val program = List(add(3), mul(4), add(5), display _, clear _, add(9), mul(2))

  println(Accumulator.accum(program))
}
