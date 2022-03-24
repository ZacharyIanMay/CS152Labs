package jedi.expression
import jedi.context.Environment

case class Identifier (val name: String) extends Expression:
  override def toString = name
  def execute(env: Environment) = env(this)