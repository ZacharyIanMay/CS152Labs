package InClass

// this is how one declares a class with three fields in Scala:
case class Note(val amplitude: Double, val frequency: Double, val duration: Double = 1.0)

// a sample score for testing purposes:
val symphony1 =
  List(Note(3, 30), Note(3.1, 40, .25), Note(3.2, 10, .5),
    Note(5.1, 5, -.75), Note(3.9, 2))

def durationIter(score: List[Note]) = {
  var result = 0.0
  for(note <- score if 0 < note.duration)
    result = result + note.duration
  result
}

def durationRec(score: List[Note]):Double =
  if (score == Nil) 0.0
  else if (0 < score.head.duration) score.head.duration + durationRec(score.tail)
  else durationRec(score.tail)

def durationTail(score: List[Note]) = {
  def helper(result: Double, unseen: List[Note]): Double =
    if (unseen == Nil) result
    else if  (0 < unseen.head.duration)
      helper(result + unseen.head.duration, unseen.tail)
    else helper(result, unseen.tail)
  helper(0.0, score)
}

def  durationPipe(score: List[Note]) = score.map(_.duration).filter(0 < _).reduce(_ + _)

def maxAmpIter(score: List[Note]): Double =
  var result = 0.0
  for(i <- score if i.amplitude > result) result = i.amplitude
  result

def maxAmpRec(score: List[Note]): Double =
  if(score.isEmpty) 0
  else
    val pop = score(0)
    val list = score.drop(1)
    val sub = maxAmpRec(list)
    if(pop.amplitude > sub) pop.amplitude
    else sub

def maxAmpTail(score: List[Note]): Double =
  def helper(list: List[Note], res: Double): Double =
    if(score.isEmpty) 0
    else
      val pop = score(0)
      val sub = score.drop(1)
      if(pop.amplitude > res) helper(sub, pop.amplitude)
      else helper(sub, res)
  helper(score, 0)

def maxAmpPipe(score:List[Note]): Double =
  score.filter(_.duration > 0).filter(_.amplitude > 0).map(_.amplitude).reduce(Math.max(_, _))


object testScores extends App{
  println(durationIter(symphony1))
  println(durationRec(symphony1))
  println(durationTail(symphony1))
  println(durationPipe(symphony1))


}
