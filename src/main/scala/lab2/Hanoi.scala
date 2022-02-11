package lab2



import scala.collection.mutable

class Hanoi {
  val numRings = 3
  val numTowers = 3
  val towers: Array[mutable.Stack[Int]] = Array.ofDim[mutable.Stack[Int]](numTowers)
  // initialize towers
  for(t <- 0 until numTowers) towers(t) = mutable.Stack[Int]()
  // push ..., 3, 2, 1 onto tower 0
  for(r <- numRings to 1 by -1) towers(0).push(r) // towers = [Stack(1, 2, 3) Stack() Stack()]

  override def toString = {
    var result = "["
    for(t <- 0 until numTowers) {
      result += towers(t).toString + " "
    }
    result + "]"
  }

  // legally move num rings from fromTower to toTower
  def move(num: Int, fromTower: Int, toTower: Int): Unit =
    def find(): Int =
      var v = 0
      for(t <- numTowers-1 to 0 by -1)
        if ((t != fromTower) && (t != toTower)) v = t
      v
    if(num == 1) {
      if (towers(toTower).length != 0 && towers(fromTower).top > towers(toTower).top) throw new Exception
      towers(toTower).push(towers(fromTower).pop())
      println(this.toString())
    }
    else {
      val a = find()
      move(num - 1, fromTower, a)
      move(1, fromTower, toTower)
      move(num - 1, a, toTower)
    }
}

object Tower extends App {
  val game = Hanoi()
  println(game.toString)
  // move 3 rings from tower 0 to tower 1
  game.move(3, 0, 2)
}

/*
Output:
[Stack(1, 2, 3) Stack() Stack() ]
[Stack(2, 3) Stack() Stack(1) ]
[Stack(3) Stack(2) Stack(1) ]
[Stack(3) Stack(1, 2) Stack() ]
[Stack() Stack(1, 2) Stack(3) ]
[Stack(1) Stack(2) Stack(3) ]
[Stack(1) Stack() Stack(2, 3) ]
[Stack() Stack() Stack(1, 2, 3) ]
*/

