package lab5
import cui._

class OutOfGas extends UserError("You are out of gas")
class Escaped extends UserError("You have escaped!")

enum Heading:
  case north, south, east, west

class Robot(val name: String):
  var fuel = 100
  var dir: Heading = Heading.east
  var loc = (0, 0)
  var gas = true
  var escaped = false

object maze extends Console with App {
  val rng = util.Random()
  var exit = (rng.nextInt(10), rng.nextInt(10))
  val robot = Robot("Robbie")
  val instruction = "Commands ::= restart | move STEPS | turn HEADING"

  println(instruction)
  println(update())

  def distance (p1: (Int, Int), p2: (Int, Int)) =
    val (a, b) = p1
    val (c, d) = p2
    (math.sqrt((a - c) * (a - c) + (b - d) * (b - d))).toInt

  def update(): String = robot.name + " at " + robot.loc + " heading " + robot.dir + " with " + robot.fuel + " units of fuel. Distance to goal = " + distance(robot.loc, exit)


  def execute(cmmd: String): String =
    val parts = cmmd.split("\\s+")
    val length = parts.length

    length match
      case 0 => throw UserError("Provide a command")
      case _ =>
    val op = parts(0)
    if(!robot.gas && parts(0) != "restart")
      "You are out of gas"
    else if(robot.escaped && parts(0) != "restart")
      robot.name + " escaped!"
    else
      parts(0) match {
        case "restart" =>
          exit = (rng.nextInt(10), rng.nextInt(10))
          robot.fuel = 100
          robot.gas = true
          robot.dir = Heading.east
          robot.escaped = false
          robot.loc = (0, 0)
          update()

        case "turn" =>
          if(parts(1).matches("north")) robot.dir = Heading.north
          else if(parts(1).matches("east")) robot.dir = Heading.east
          else if(parts(1).matches("south")) robot.dir = Heading.south
          else if(parts(1).matches("west")) robot.dir = Heading.west
          else throw UserError("Invalid heading")
          val name = robot.name
          update()

        case "move" =>
          if(parts.size < 2) throw UserError("Provide a number of steps")
          if(!parts(1).matches("\\d+")) throw UserError("Steps must be nubmer")
          val steps: Int = parts(1).toInt
          robot.dir match {
            case Heading.north => robot.loc = (robot.loc(0) + steps, robot.loc(1))
            case Heading.south => robot.loc = (robot.loc(0) - steps, robot.loc(1))
            case Heading.east => robot.loc = (robot.loc(0), robot.loc(1) + steps)
            case Heading.west => robot.loc = (robot.loc(0), robot.loc(1) - steps)
          }
          robot.fuel = robot.fuel - steps
          if(robot.loc == exit)
            robot.escaped = true
            robot.name + " escaped!"
          else if(robot.fuel <= 0)
            robot.gas = false
            "You are out of gas"
          else update()

        case _ => throw UserError(s"unrecognized command: $op")

        }

  repl() // start the repl

}

