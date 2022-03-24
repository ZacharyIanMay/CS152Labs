package jedi.context
import jedi.value.Value
import jedi.expression.Identifier
import jedi.context.Environment

class Environment extends collection.mutable.HashMap[Identifier, Value]