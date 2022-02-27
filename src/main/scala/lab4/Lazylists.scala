package lab4

object Lazylists extends App
{
  def inf1(): LazyList[Int] = 1 #:: inf1()
  def allint(c: Int): LazyList[Int] = c #:: allint(c+1)
  val ints = allint(0)
  val posevenints = ints.filter(_ % 2 == 0)
  val sqints = ints.map((i: Int)=>i*i)
}
