package lab5

import lab5.Transaction.{getID, idGen}
// ++++++++++++++++++++++++
// Implementing a reference class
// ++++++++++++++++++++++++

// given
enum LetterGrade:
  case A, B, C, D, F

class Assignment(private val name: String, private val number: Int, private var _grade: Int):
  if (_grade < 0 || _grade > 100) throw Exception("Invalid grade")
  var letterGrade: LetterGrade = LetterGrade.F
  if (_grade >= 90) letterGrade = LetterGrade.A
  else if (_grade >= 80) letterGrade = LetterGrade.B
  else if (_grade >= 70) letterGrade = LetterGrade.C
  else if (_grade >= 60) letterGrade = LetterGrade.D
  else letterGrade = LetterGrade.F
  override def toString = name + " assn " + number + ": " + _grade + " (= " + letterGrade + ")"
  def grade = _grade
  def grade_=(ng: Int) =
    if (ng < 0 || ng > 100) throw Exception("Invalid grade")
    _grade = ng
    if (ng >= 90) letterGrade = LetterGrade.A
    else if (ng >= 80) letterGrade = LetterGrade.B
    else if (ng >= 70) letterGrade = LetterGrade.C
    else if (ng >= 60) letterGrade = LetterGrade.D
    else letterGrade = LetterGrade.F



object testAssignment extends App:

  try
    val simpson = Assignment("Simpson", 1, -88)
  catch
    case e: Exception => println(e.getMessage) // Invalid grade

  val jones = Assignment("Jones", 1, 88)
  val hanson = Assignment("Hanson", 1, 95)

  println(jones.grade) // 88
  println(jones.letterGrade) // B
  jones.grade = jones.grade + 10
  println(jones.grade) //98
  println(jones.letterGrade) // A
  println(jones) // Jones assn 1: 98 (= A)

  println(hanson.grade) // 95
  println(hanson.letterGrade) // A
  try
    hanson.grade = hanson.grade + 10
  catch
    case e: Exception => println(e.getMessage) // Invalid grade
  finally
    println(hanson) // Hanson assn 1: 95 (= A)
  try
    val smith = Assignment("Smith", 1, -10)
  catch
    case e: Exception => println(e.getMessage) // Invalid grade

// ++++++++++++++++++++++++
// Implementing static variables & methods
// ++++++++++++++++++++++++

object Transaction:
  var idGen: Int = 500
  def getID(): Int =
    idGen = idGen + 1
    idGen - 1

class Transaction(val fromAccount: Int, val toAccount: Int, val amount: Double):
  if(amount < 0) throw Exception("Invalid amount")
  val id: Int = getID()
  override def toString = s"Tansaction #$id: $amount from acct $fromAccount to acct $toAccount"

object testTransactions extends App:
  try
    val t1 = Transaction(119, 212, -20.50)
  catch
    case e: Exception => println(e.getMessage) // Invalid amount
  val ledger = List(
    Transaction(119, 212, 600.50),
    Transaction(212, 119, 1200),
    Transaction(212, 119, 98.75)
  )
  ledger.foreach(println) // how to create unique IDs
/*
Transaction #500: $600.5 from acct 119 to acct 212
Transaction #501: $1200.0 from acct 212 to acct 119
Transaction #502: $98.75 from acct 212 to acct 119
*/

// ++++++++++++++++++++++++
// Implementing a value class
// ++++++++++++++++++++++++

class Time(val hour: Int, val minute: Int) extends Ordered[Time]:
  if(hour > 24 || hour < 0) throw Exception("Invalid hour")
  if(minute > 60 || minute < 0) throw Exception("Invalid minute")
  override def hashCode() = hour * 100 + minute
  def this(str: String) = this(str.split(":")(0).toInt, str.split(":")(1).toInt)
  def this(hour: Int) = this(hour, 0)
  override def toString() =
    var h = s"$hour:"
    if(hour < 10) h = s"0$hour:"
    if(minute < 10) h = h + s"0$minute"
    else h = h + s"$minute"
    h
  override def compare(that: Time): Int = this.hashCode() - that.hashCode()
  def +(that: Time): Time = Time((this.hour + that.hour) % 24, (this.minute + that.minute) % 60)
  override def equals(obj: Any): Boolean = this.hashCode() == obj.hashCode()



class PreciseTime(hour: Int, minute: Int, val second: Int) extends Time(hour, minute):
  if(second > 60 || second < 0) throw Exception("Invalid second")
  def this(hour: Int) = this(hour, 0, 0)
  def this(hour: Int, minute: Int) = this(hour, minute, 0)
  override def hashCode() = 100000 + hour * 10000 + minute * 100 + second
  override def toString() =
    val b = super.toString()
    val s = s":$second"
    if (second < 10) b + s":0$second"
    else b + s

object testTime extends App:
  try
    val t = Time(24, 50)
  catch
    case e: Exception => println(e.getMessage) // Invalid hour
  try
    val t = Time(12, 60)
  catch
    case e: Exception => println(e.getMessage) // Invalid minute

  val t1 = Time(10, 30)
  val t2 = Time(15, 45)
  val t3 = Time(10, 30)
  val t4 = Time("18:45")
  val t5 = Time(17)
  val t6 = t1 + t2
  println(t1) // 10:30
  println(t2) // 15:45
  println(t3) // 10:30
  println(t4) // 18:45
  println(t5) // 17:00
  println(t6) // 2:15
  println(t1 == t3) // true
  println(t1 != t5) // true
  println(t1 < t2)  // true
  println(t4 < t2)  // false
  println(t1 <= t3) // true

  val schedule = Map(
    t1 -> "coffee break",
    t2 -> "nap",
    t4 -> "cocktail hour"
  )
  println(schedule) // Map(10:30 -> coffee break, 15:30 -> nap, 18:45 -> cocktail hour)
  println(schedule(t3)) // coffee break
  try
    val pt = PreciseTime(12, 0, 60)
  catch
    case e: Exception => println(e.getMessage) // Invalid second

  val pt2 = PreciseTime(18)
  val pt1 = PreciseTime(10, 30)
  println(pt1) // 10:30:00
  println(pt2) // 18:00:00
  println(t1) // 10:30
  println(pt1 == t1) // false
  try
    println(schedule(pt1))
  catch
    case e: Exception => println(e.getMessage) // key not found: 10:30:00