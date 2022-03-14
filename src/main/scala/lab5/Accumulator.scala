package lab5

trait Instruction:
  def execute(): Unit

object Accumulator {
  var halt = false
  var register = 0.0
  var program = List[Instruction]()
  var inst = 0
  def run(): Double =
    halt = false
    inst = 0
    register = 0
    while(!halt && inst < program.size)
      program(inst).execute()
      inst = inst + 1
    register
}

class Add(v: Double) extends Instruction:
  def execute() = Accumulator.register = Accumulator.register + v

class Mul(v: Double) extends Instruction:
  def execute() = Accumulator.register = Accumulator.register * v

class Halt() extends Instruction:
  def execute() = Accumulator.halt = true

class Rep(num: Int, body: Instruction) extends Instruction:
  def execute() = for(i <- 0 until num) body.execute()

class Blt(req: Double, jmp: Int) extends Instruction:
  def execute() = if(Accumulator.register < req) Accumulator.inst = Accumulator.inst + jmp - 1


object testAccumulator extends App:
  // computing 3 * 4 + 9
  Accumulator.program = List(Add(3), Mul(4), Add(9))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 21
  // computing ((3 * 5) + 1) * 2
  Accumulator.program = List(Add(3), Mul(5), Add(1), Mul(2))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 32
  // computing (((10 * 2) + 3) * 5)
  Accumulator.program = List(Add(10), Mul(2), Add(3), Mul(5))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 115
  Accumulator.program = List(Add(3), Mul(4), Halt(), Add(9))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 12
  Accumulator.program = List(Add(1), Rep(5, Mul(2)), Add(10))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 42
  Accumulator.program = List(Add(1), Rep(5, Mul(2)), Blt(33, 2), Add(10), Halt(), Add(10))
  Accumulator.run()
  println("register = " + Accumulator.register) // prints 32