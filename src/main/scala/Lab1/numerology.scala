package Lab1

object numerology extends App {

  // problem 1
  def kingdom(n: Int): Int =
    if(n % 100 == 0) 2
    else if(n <= 10) 3
    else if(n % 2 == 0) 1
    else 4

  // testing
  println(s"kingdom(11) = ${kingdom(11)}")   // 4
  println(s"kingdom(16) = ${kingdom(16)}")   // 1
  println(s"kingdom(5) = ${kingdom(5)}")     // 3
  println(s"kingdom(200) = ${kingdom(200)}") // 2



  // problem 3

  // original version
//  def species(n: Int) =
//    if (0 < n) if (n % 2 == 0) 1 else 2

  // corrected version
  def species2(n: Int): Int =
    if (n > 0)
      if(n % 2 == 0) return 1
    2



  // problem 4

  // odd positives are realm 1
  def realm1(n: Int): Int =
    if(n > 0)
      if(n % 2 == 1) return 1
    throw new Exception()

  // even positives not divisible by 3 are realm 2
  def realm2(n: Int): Int =
    if(n > 0)
      if(n % 2 == 0)
        if(n % 3 != 0) return 2
    throw new Exception()

  // even positives divisible by 6 and 7 are realm 3
  def realm3(n: Int): Int =
    if(n > 0)
      if(n % 2 == 0)
        if(n % 6 == 0)
          if(n % 7 == 0) return 3
    throw new Exception()

  def realm(n: Int): Int =
    try {
      realm1(n)
    } catch {
      case e: Exception => try {
        realm2(n)
      } catch {
        case e: Exception => try {
          realm3(n)
        } catch {
          case e: Exception => 0
        }
      }
    }


  println(s"realm(0) = ${realm(0)}")   // 0
  println(s"realm(5) = ${realm(4)}")   // 2
  println(s"realm(42) = ${realm(42)}") // 3
  println(s"realm(9) = ${realm(9)}")   // 1



  // problem 5

  // odd positives are realm 1
  def realm1Opt(n: Int): Option[Int] =
    if(n > 0)
      if(n % 2 == 1) Some(1)
      else None
    else None

  // even positives not divisible by 3 are realm 2
  // another style: check for bad news first
  def realm2Opt(n: Int): Option[Int] =
    if(n > 0)
      if(n % 2 == 0)
        if(n % 3 != 0) Some(2)
        else None
      else None
    else None

  // even positives divisible by 6 and 7 are realm 3
  def realm3Opt(n: Int): Option[Int] =
    if(n > 0)
      if(n % 2 == 0)
        if(n % 6 == 0)
          if(n % 7 == 0) Some(3)
          else None
        else None
      else None
    else None

  def realmOpt(n: Int) =
    realm1Opt(n) match {
    case None => realm2Opt(n) match {
      case None => realm3Opt(n) match {
        case None => 0
        case Some(3) => 3
      }
      case Some(2) => 2
    }
    case Some(1) => 1
  }

  println(s"realmOpt(0) = ${realmOpt(0)}")   // 0
  println(s"realmOpt(5) = ${realmOpt(4)}")   // 2
  println(s"realmOpt(42) = ${realmOpt(42)}") // 3
  println(s"realmOpt(9) = ${realmOpt(9)}")   // 1



}
