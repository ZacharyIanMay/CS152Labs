package lab3

import scala.annotation.tailrec


class DDS:

  // problem 1
  @tailrec
  final def controlLoop[S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S): S =
    if(halt(state, cycle)) state
    else controlLoop(update(state, cycle), cycle + 1, halt, update)

  // problem 2
  def h2(x: Int, cycle: Int): Boolean = if(x > (10^5)) true else false
  def u2(x: Int, cycle: Int): Int = x*2

  val population: Int = controlLoop(1, 1, h2, u2)

  // problem 3
  def solve(f: Double => Double): Double =
    val delta = 1e-7
    def goodEnough(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
    def df(x: Double) = (f(x+delta) - f(x))/delta
    def improve(guess: Double, cycle: Int) = guess - f(guess)/df(guess)
    controlLoop(1.0, 0, goodEnough, improve)

  // problem 4
  def sqrt(x: Double): Double =
    def g(n: Double) = n * n - x
    solve(g)

  // problem 5
  def cubeRoot(x: Double): Double =
    def g(n: Double) = n * n * n - x
    solve(g)

  // problem 6
  def nthRoot(x: Double, n: Int): Double =
    @tailrec
    def g(b: Double, v: Double, c: Int): Double =
      if(c == 1) v
      else g(b, b * v, c-1)
    def f(y: Double) = g(y, y, n) - x
    solve(f)

  // problem 7

  // = value of an investment of $principle at an anual rate r compounded
  // periods times over 1 year
  def value(principle: Double, rate: Double, periods: Int): Double =
    def h1(x: Double, cycle: Int): Boolean = if(cycle >= periods) true else false
    def u1(x: Double, cycle: Int): Double = x * (1+rate/periods)
    controlLoop(principle, 0, h1, u1)

object TestDDS extends DDS with App:
  println(value(50, .1, 12))
  println(nthRoot(64, 3))
  println(nthRoot(16, 4))
  // etc.