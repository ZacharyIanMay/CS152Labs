package Lab1

import scala.io.*

class UserError(gripe: String) extends Exception(gripe)

abstract class Console {

  // override in an extension
  def execute(cmmd: String): String
  //def executeFile(f: File): String

  //def run(f: File): Unit = replF(f)

  def run(f: Array[String]): Unit = repl

  def repl: Unit =
    var loop = true
    var command = ""
    while(loop)
      try
        command = StdIn.readLine("-> ")
        if(command == "!q")
          loop = false
        else
          println(execute(command))
      catch
        case u: UserError => println(u.getMessage)
        case e: Exception =>
          println(e.getMessage)
          e.printStackTrace()
          loop = false

  /*
  def replF(f: File): Unit =
    var loop = true
    var command = ""
    while(loop)
      try
        command = StdIn.readLine(f)
        if(command == "!q")
          loop = false
        else
          println(execute(command))
      catch
        case u: UserError => println(u.getMessage)
        case e: Exception => println(e.getMessage); e.printStackTrace; loop = false
  */
}

class MathConsole extends Console {

  def digits(part: String) = part.matches("\\d+")

  def execute(cmmd: String): String =
    println(cmmd)
    val parts = cmmd.split("\\s+")
    val length = parts.length

    length match
      case 0 => throw UserError("Provide a command")
      case 1 => throw UserError("Provide at least 1 argument")
      case _ =>
    val op = parts(0)
    var result = 0.0
    parts(0) match {
      case "add" =>
        for(i <- 1 until parts.length)
          if(!parts(i).matches("\\d+")) throw UserError("Arguments must be nubmers")
          result = result + parts(i).toDouble
        result.toString

      case "sub" =>
        if(!parts(1).matches("\\d+")) throw UserError("Arguments must be nubmers")
        result = parts(1).toDouble
        for(i <- 2 until parts.length)
          if(!parts(i).matches("\\d+")) throw UserError("Arguments must be nubmers")
          result -= parts(i).toDouble
        result.toString

      case "mul" =>
        if(!parts(1).matches("\\d+")) throw UserError("Arguments must be nubmers")
        result = parts(1).toDouble
        for(i <- 2 until parts.length)
          if(!parts(i).matches("\\d+")) throw UserError("Arguments must be nubmers")
          result *= parts(i).toDouble
        result.toString

      case "div" =>
        if(!parts(1).matches("\\d+")) throw UserError("Arguments must be nubmers")
        result = parts(1).toDouble
        for(i <- 2 until parts.length)
          if(!parts(i).matches("\\d+")) throw UserError("Arguments must be nubmers")
          if(parts(i).toDouble.equals(0))
            throw UserError("No division by 0")
          result /= parts(i).toDouble
        result.toString

      case _ => throw UserError(s"Unrecognized operator:$op")

    }

}

object MathConsole {
  def main(args: Array[String]): Unit = {
    val cui = MathConsole()
    cui.run(args)
  }
}


