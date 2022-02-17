package lab3

class DDS:
  final def controlLoop[S](State: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S): S =
    if(halt(state, cycle)) state
    else controlLoop(update(state, cycle), cycle + 1, halt, update)


  def solve(f: Double => Double): Double =
    val delta = 1e-7
    def goodEnough(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
    def df(x: Double) = (f(x+delta) - f(x))/delta
    def improve(guess: Double, cycle: Int) = guess - f(guess)/df(guess)
    controlLoop(1.0, 0, goodEnough, improve)