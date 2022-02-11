package Lab1

class ArithmeticUtils {

  def sqrt(n: Int) = {
    if (n < 0) None
    else {
      var result = 0
      for(i <- 0 until n/2 if i * i <= n) {
        result = i
      }
      Some(result)
    }
  }

  def log(n: Int) = {
    if (n <= 0) None
    else {
      var result = 1
      var pow = 1
      while(2 * pow <= n) {
        result += 1
        pow = 2 * pow
      }
      Some(result)
    }
  }

  def isPrime(n: Int) = {
    if (n < 0) None
    else if (n < 2) Some(false)
    else {
      var result = true
      for(i <- 2 until sqrt(n).get if result) result = (n % i != 0)
      Some(result)
    }
  }

  def gcd(n: Int, m: Int): Option[Int] = {
    if (m < 0 || n < 0) None
    else if (m == 0 || n == 0) Some(0)
    else {
      var result = 1
      for(i <- 2 to math.min(n, m) if m % i == 0 && n % i == 0) result = i
      Some(result)
    }
  }

  def lcm(n: Int, m: Int) = {
    if (m < 0 || n < 0) None
    else if (m == 0 || n == 0) Some(0)
    else {
      var result = 1
      var done = false
      for(i <- 2 to m * n if !done && i % m == 0 && i % n == 0) {
        result = i
        done = true
      }
      Some(result)
    }
  }

  def phi(n: Int) = {
    if (n < 0) None
    var result = 0
    for(i <- 1 to n)
      if (gcd(n, i).get == 1) result += 1;
    Some(result)
  }


}

object Arithmetic extends ArithmeticUtils with App {
  println("gcd(15, 12) = " + gcd(15, 12))
  println("lcm(15, 12) = " + lcm(15, 12))
  println("gcd(13, 12) = " + gcd(13, 12))
  println("gcd(-13, 12) = " + gcd(-13, 12))
  println("phi(9)= " + phi(9))
  println("sqrt(49) = " + sqrt(49))
  println("sqrt(37) = " +sqrt(37))
  println("sqrt(35) = " + sqrt(35))
  println("log(64) = " + log(64))
  println("log(130) = " + log(130))
  println("log(9) = " + log(9))
  println("log(0) = " + log(0))
  println("isPrime(23) = " + isPrime(23))
  println("isPrime(59) = " + isPrime(59))
  println("isPrime(75) = " + isPrime(75))
}

/*
output:

gcd(15, 12) = Some(3)
lcm(15, 12) = Some(60)
gcd(13, 12) = Some(1)
gcd(-13, 12) = None
phi(9)= Some(6)
sqrt(49) = Some(7)
sqrt(37) = Some(6)
sqrt(35) = Some(5)
log(64) = Some(7)
log(130) = Some(8)
log(9) = Some(4)
log(0) = None
isPrime(23) = Some(true)
isPrime(59) = Some(true)
isPrime(75) = Some(false)

*/