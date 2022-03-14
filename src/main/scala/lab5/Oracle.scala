package lab5
import cui.*
import scala.collection.mutable.*

trait Learner:
  def tell(query: String, answer: String) = "Got it!"

trait Oracle extends Learner:
  val dunno = "Sorry, I don't know"
  var debug = true
  def ask(query: String): String =
    if (debug) println("asking oracle")
    dunno

object QueryConsole extends Console with App with MagicOracle with MathOracle with GeoOracle with BioOracle {
  override def execute(query: String): String =
    val eqPos = query.indexOf("=")
    var isFact = ( eqPos != -1)
    if (isFact) {
      tell(query.substring(0, eqPos).trim, query.substring(eqPos + 1).trim)
    } else {
      ask(query.trim)
    }
  repl()
}

trait MathOracle extends Oracle {
  override def ask(query: String): String =
    if(debug) println("asking math oracle")
    try{
      eval(query)
    }
    catch{
      case e: Exception => super.ask(query)
    }

  private def eval(query: String): String = {
    val tokens = query.split("\\s+")
    if (tokens.length <= 1) throw UserError("Must have at least one argument")
    val args = tokens.drop(1).map(_.toDouble) // might throw here too
    var result = 0.0
    tokens(0) match {
      case "add" => for (arg <- args) result += arg; result.toString
      case "sub" => for (arg <- args) result -= arg; result.toString
      case "mul" => result = 1; for (arg <- args) result *= arg; result.toString
      case "div" => result = 1; for (arg <- args) result /= arg; result.toString
      case _ => super.ask(query)
    }
  }
}

trait MagicOracle extends Oracle{
  val responses: List[String] = List("It is certain", "It is decidedly so", "Without a doubt", "Yes - definitely", "You may rely on it", "As I see it, yes", "Most likely", "Outlook good", "Yes", "Signs point to yes", "Reply hazy, try again", "Ask again later", "Better not tell you now", "Cannot predict now", "Concentrate and ask again", "Don't count on it", "My reply is no", "My sources say no", "Outlook not so good", "Very Doubtful")
  override def ask(query: String): String =
    if(debug) println("asking magic oracle")
    val qpos = query.indexOf("?")
    if(qpos != -1)
      val r = util.Random()
      responses(r.nextInt(20))
    else super.ask(query)
}


trait KbaseOracle extends Oracle {
  var kbase: Map[String, String] = Map[String, String]()

  override def ask(query: String): String =
    if(debug) println("asking kbase oracle")
    try{
      kbase(query)
    }
    catch{
      case e: Exception => super.ask(query)
    }

  override def tell(query: String, answer: String) =
    kbase(query) = answer
    super.tell(query, answer)
}

trait GeoOracle extends KbaseOracle {
  kbase("capitol of CA") = "Sacramento"
  kbase("capitol of VA") = "Richmond"
  kbase("capitol of NY") = "Albany"
  kbase("capitol of TX") = "Austin"
}

trait BioOracle extends KbaseOracle {
  kbase("giraffe is a") = "mammal"
  kbase("ant is a") = "insect"
  kbase("snake is a") = "reptile"
  kbase("tuna is a") = "fish"
}