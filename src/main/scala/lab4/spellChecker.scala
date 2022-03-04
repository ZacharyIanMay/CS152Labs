package lab4

class spellCheck {

  def spellCheckRec(doc: String, dict: List[String]): Int =
    val list = doc.split("\\W+")
    if(list.isEmpty) return 0
    else
      val pop = list(0)
      val rem = list.drop(1)
      val str = list.reduce(_ + " " + _)
      if(dict.contains(doc)) spellCheckRec(str, dict)
      else 1 + spellCheckRec(str, dict)

  def spellCheck(doc: String, dict: List[String]): Int =
    def help(a: String): Int = if(dict.contains(a)) 0 else 1
    doc.split("\\W+").filter(_.matches("[a-zA-Z]+")).map(_.toLowerCase).map(help).reduce(_+_)

}
