package lab2

object Choice extends App {

  // # ways to choose m from n
  def choose(n: Int, m: Int): Int =
    if(n < 0 || m < 0) throw new Exception
    def helper(v: Int, c: Int): Int =
      if(n == m) 1
      else if(m == 0) 1
      else helper(v * (n - c), c-1)
    helper(0, 0)

  // Pascal's Triangle
  for (i <- 0 to 10)
    for (j <- 0 to i)
      print(choose(i, j) + "\t\t")
    println

}

/*
output

1
1		1
1		2		1
1		3		3		1
1		4		6		4		1
1		5		10		10		5		1
1		6		15		20		15		6		1
1		7		21		35		35		21		7		1
1		8		28		56		70		56		28		8		1
1		9		36		84		126		126		84		36		9		1
1		10		45		120		210		252		210		120		45		10		1
*/

