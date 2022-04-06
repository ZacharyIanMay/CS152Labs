package jedi.value

import jedi.context.TypeException
import jedi.expression.Literal

object Boole:
  val TRUE = true
  val FALSE = false

case class Boole(value: Boolean) extends Literal:
  def &&(other: Value): Boole =
    other match
      case x: Boole => Boole(this.value && x.value)
      case _ => throw new TypeException("Arguments must be a Boole")

  def ||(other: Value): Boole =
    other match
      case x: Boole => Boole(this.value || x.value)
      case _ => throw new TypeException("Arguments must be a Boole")

  def unary_! : Boole =
    Boole(!this.value)
