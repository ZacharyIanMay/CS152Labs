package lab4

class Accumulator {

  def accum(program: List[Int => Int]): Int =
    var register = 0
    for(i <- program) i(register)
    register
}
