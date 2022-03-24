package Midterm

//                                    Midterm Solutions

// ++++++++++++++++++++++++++++++++
// Problem 1 : countRoots
// ++++++++++++++++++++++++++++++++

def countRoots(f: Double => Double, input: List[Double]): Int =
  input.map(f(_)).filter(_==0).size

@main def testCountRoots() =
  def f(x: Double) = (x - 5) * (x + 3) // roots are 5 and -3
  println(countRoots(f, List(2.0, 3.0, 5.0))) // = 1
  println(countRoots(f, List(2.0, 3.0, 4.0))) // = 0
  println(countRoots(f, List(-3.0, 0.0, 3.0, 5.0))) // = 2

// ++++++++++++++++++++++++++++++++
// Problem 2 optionMap
// ++++++++++++++++++++++++++++++++

def optionMap[T, V](in: List[T], f: T=>Option[V]): List[V] =
  in.map(f(_)).filter(!_.isEmpty).map(_.get)

@main def testOptionMap() =
  def invert(x: Double) =  if (x != 0) Some(1/x) else None
  println(optionMap(List(2.0, 8.0, 0.0, 4.0), invert)) // = List(0.5, 0.125, 0.25)

// ++++++++++++++++++++++++++++++++
// Problem 3 Avatar
// ++++++++++++++++++++++++++++++++

class Avatar(val name: String, private var _weapon: Int):
  private var _health = 100
  if(_weapon > 3 || _weapon < 0) throw Exception("Invalid Weapon")
  var status: String = "strong"
  var weaponString: String = "no weapon"
  if(_health == 0) status = "dead"
  else if(_health <= 50) status = "weak"
  else status = "strong"
  _weapon match {
    case 0 => weaponString = "no weapon"
    case 1 => weaponString = "a sword"
    case 2 => weaponString = "a gun"
    case 3 => weaponString = "a grenade"
  }
  def this(name: String) = this(name, 0)
  def health_=(h: Int) =
    if(h > 100) _health = 100
    else if(h < 0) _health = 0
    else _health = h
    if(_health == 0) status = "dead"
    else if(_health <= 50) status = "weak"
    else status = "strong"
  def health_-=(h: Int) =
    _health = _health - h
    if(_health < 0) _health = 0
  def health = _health
  def weapon = _weapon
  def weapon_=(wep: Int) =
    if(wep > 3 || wep < 0) throw Exception("Invalid Weapon")
    else _weapon = wep
    _weapon match {
      case 0 => weaponString = "no weapon"
      case 1 => weaponString = "a sword"
      case 2 => weaponString = "a gun"
      case 3 => weaponString = "a grenade"
    }
  override def toString: String = s"$name is $status & holds $weaponString"
  override def hashCode(): Int = this.toString.hashCode()

@main def testAvatar =
  val avatar1 = Avatar("Wandorf", 1)
  println(avatar1.toString) // Wandorf is strong & holds a sword
  avatar1.health -= 60
  avatar1.weapon = 2
  println(avatar1.toString) // Wandorf is weak & holds a gun
  println(avatar1.name + "'s health = " + avatar1.health) // Wandorf's health = 40
  println(avatar1.name + "'s status = " + avatar1.status) // Wandorf's status = weak
  avatar1.health -= 60
  avatar1.weapon = 0
  println(avatar1.name + "'s health = " + avatar1.health) // Wandorf's health = 0
  println(avatar1.toString) // Wandorf is dead & holds no weapon
  avatar1.health = 1000
  avatar1.weapon = 3
  println(avatar1.name + "'s health = " + avatar1.health)
  println(avatar1.toString) // Wandorf is strong & holds a grenade
  try
    avatar1.weapon = 4
  catch
    case e: Exception => println(e.getMessage) // Invalid Weapon
  try
    val avatar2 = Avatar("Stimpy", 4)
  catch
    case e: Exception => println(e.getMessage) // Invalid Weapon

