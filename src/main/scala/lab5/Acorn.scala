package lab5

trait Expression:
  def execute: Double

class Sum(val a: Expression, val b: Expression) extends Expression:
  def execute = a.execute + b.execute

class Prod(val a: Expression, val b: Expression) extends Expression:
  def execute = a.execute * b.execute

class Num(val d: Double) extends Expression:
  def execute = d

object AcornDemo extends App {
  println(Num(3.14).execute) // = 3.14
  println(Sum(Num(5), Num(10)).execute) // = 5 + 10 = 15.0
  println(Prod(Num(5), Num(10)).execute) // = 5 * 10 = 50.0
  println(Sum(Num(12), Prod(Num(5), Num(3))).execute) // = 12 + (5 * 3) = 27.0
  println(Prod(Num(7), Sum(Num(8), Num(3))).execute) // = 7 * (8 + 3) = 77.0
}