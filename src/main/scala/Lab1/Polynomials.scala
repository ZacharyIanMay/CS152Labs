package Lab1
import scala.math._

class PolynomialUtils {

  def roots(p: (Double, Double, Double)): Option[(Double, Double)] =
    val root = p(1)*p(1) - 4*p(0)*p(2)
    val div = 2 * p(0)
    if (root < 0) None
    else
      val A = (-p(1) + sqrt(root))/div
      val B = (-p(1) - sqrt(root))/div
      Some(A, B)

  def deriv(p: (Double, Double, Double)): (Double, Double, Double) =
    var result = (0.0, 0.0, 0.0)
    result = (0, 2*p(0), p(1))
    result

  def eval(a: Double, p: (Double, Double, Double)): Double = p(0)*a*a + p(1)*a + p(2)

  def toString(p: (Double, Double, Double)): String =
    var result: String = ""
    val one = p(0) != 0
    val two = p(1) != 0
    val three = p(2) != 0
    if(one) result += s"${p(0)}x^2"
    if(one && two || one && three) result += " + "
    if(two) result += s"${p(1)}x"
    if(two && three) result += " + "
    if(three) result += s"${p(2)}"
    result

}


object Polynomials extends PolynomialUtils with App {
  val poly = (3.0, 9.0, -30.0) // = (3x - 6) * (x + 5)

  println("poly = " + toString(poly))
  println("eval(6, poly) = " + eval(6, poly))
  println("eval(2, poly) = " + eval(2, poly))
  println("eval(-5, poly) = " + eval(-5, poly))

  println("roots(poly) = " + roots(poly))

  println("deriv(poly) = " + toString(deriv(poly)))
  println("deriv2(poly) = " + toString(deriv(deriv(poly))))

}

/*
output
poly = 3.0x^2 + 9.0x + -30.0
eval(6, poly) = 132.0
eval(2, poly) = 0.0
eval(-5, poly) = 0.0
roots(poly) = Some((2.0,-5.0))
deriv(poly) = 6.0x + 9.0
deriv2(poly) = 6.0
*/
