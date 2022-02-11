package lab2

object Tournament extends App {

  // prob A wins if A needs n wins and B needs m
  def probability(n: Int, m: Int): Double =
    if(n < 0 || m < 0) throw new Exception
    else if(n == 0) 1
    else if(m == 0) 0
    else (probability(n-1, m)/2) + (probability(n, m-1)/2)

  // Team A sweeps the World Series
  for(i <- 4 to 0 by -1)
    println("probability A wins = " + probability(i, 4))
}

/*
Output

probability A wins = 0.5
probability A wins = 0.65625
probability A wins = 0.8125
probability A wins = 0.9375
probability A wins = 1.0

*/
