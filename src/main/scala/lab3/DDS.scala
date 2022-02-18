package lab3


class DDS:

  // problem 1
  final def controlLoop[S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S): S =
    if(halt(state, cycle)) state
    else controlLoop(update(state, cycle), cycle + 1, halt, update)

  // problem 2
  def h2(x: Int, cycle: Int) = if(x > (10^5)) true else false
  def u2(x: Int, cycle: Int) = x*2

  val population = controlLoop(1, 1, h2, u2)

  // problem 3
  def solve(f: Double => Double): Double =
    val delta = 1e-7
    def goodEnough(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
    def df(x: Double) = (f(x+delta) - f(x))/delta
    def improve(guess: Double, cycle: Int) = guess - f(guess)/df(guess)
    controlLoop(1.0, 0, goodEnough, improve)

  // problem 4
  def sqrt(x: Double) =
    def g(n: Double) = n * n - x
    solve(g _)

  // problem 5
  def cubeRoot(x: Double) =
    def g(n: Double) = n * n * n - x
    solve(g _)

  // problem 6
  def nthRoot(x: Double, n: Int) =
    def g(b: Double, v: Double, c: Int): Double =
      if(c == 1) v
      else g(b, b * v, c-1)
    def f(y: Double) = g(y, y, n) - x
    solve(f _)

  // problem 7

  // = value of an investment of $principle at an anual rate r compounded
  // periods times over 1 year
  def value(principle: Double, rate: Double, periods: Int): Double = ???

object TestDDS extends DDS with App:
  println(nthRoot(4, 2))
  println(nthRoot(64, 3))
  println(nthRoot(16, 4))
  // etc.