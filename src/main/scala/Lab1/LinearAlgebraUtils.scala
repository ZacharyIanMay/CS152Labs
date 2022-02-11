package Lab1

class LinearAlgebraUtils {

  def dim(mat: Array[Array[Double]]): (Int, Int) =
    val m = mat.length
    val n = mat(0).length
    for(i <- 0 until m-1) if(mat(i).length != n) throw new Exception("Dimensions don't match")
    (m, n)


  def dot(vec1: Array[Double], vec2: Array[Double]): Double =
    if(vec1.length != vec2.length) throw new Exception("Dimensions don't match")
    var result = 0.0
    for(i <- vec1.indices) result = result + vec1(i) * vec2(i)
    result


  def product(mat: Array[Array[Double]], vec: Array[Double]): Array[Double] =
    val dimension = dim(mat)
    if(dimension(1) != vec.length) throw new Exception("Dimensions don't match")
    val result: Array[Double] = new Array[Double](dimension(0))
    for(i <- 0 until dimension(0))
      for(j <- 0 until dimension(1))
        result(i) += mat(i)(j) * vec(j)
    result



  def transpose(mat: Array[Array[Double]]): Array[Array[Double]] =
    val dimension = dim(mat)
    val result: Array[Array[Double]] = new Array[Array[Double]](dimension(1))
    for(i <- result.indices)
      result(i) = new Array[Double](dimension(0))
    for(i <- 0 until dimension(0))
      for(j <- 0 until dimension(1))
        result(j)(i) = mat(i)(j)
    result


  def sum(vec1: Array[Double], vec2: Array[Double]): Array[Double] =
    if(vec1.length != vec2.length) throw new Exception("Dimensions don't match")
    val result: Array[Double] = new Array[Double](vec1.length)
    for(i <- vec1.indices)
      result(i) = vec1(i) + vec2(i)
    result



  def sum(mat1: Array[Array[Double]], mat2: Array[Array[Double]]): Array[Array[Double]]  =
    val dimension = dim(mat1)
    val result: Array[Array[Double]] = new Array[Array[Double]](dimension(0))
    if(dimension != dim(mat2)) throw new Exception("Dimensions don't match")
    for(i <- 0 until dimension(0))
      result(i) = sum(mat1(i), mat2(i))
    result


  def trace(mat: Array[Array[Double]]): Double =
    val dimension = dim(mat)
    if(dimension(0) != dimension(1)) throw new Exception("Dimensions don't match")
    var result = 0.0
    for(i <- 0 until dimension(0))
      result += mat(i)(i)
    result



  def toString(vec: Array[Double]): String =
    var result = "["
    for (v <- vec.indices)
      result += vec(v) + " "
    result = result.substring(0, result.length-1) + "]"
    result



  def toString(mat: Array[Array[Double]]): String =
    var result = ""
    for (vec <- mat) result = result + toString(vec) + "\n"
    result = result.substring(0, result.length - 1)
    result

}

object LinearAlgebra extends LinearAlgebraUtils with App {
  try {
    def vec1 = Array(5.0, 2.0, 6.0)

    def mat1 = Array(Array(1.0, 2.0, 3), Array(4.0, 5, 6), Array(7.0, 8, 9))

    def mat2 = Array(Array(10.0, 11, 12), Array(13.0, 14, 15), Array(16.0, 17, 18))

    println("vec1 = " + toString(vec1))
    println("mat1 = ")
    println(toString(mat1))
    println("mat2 = ")
    println(toString(mat2))
    println("dim(mat1) = " + dim(mat1))
    println("trace(mat1) = " + trace(mat1))
    println("transpose(mat2) = ")
    println(toString(transpose(mat2)))
    println("sum(mat1, mat2) = ")
    println(toString(sum(mat1, mat2)))
    println("product(mat1, vec1) = " + toString(product(mat1, vec1)))

    val vec2 = Array(0.0, 4.0, 9.0, 8.0)
    println("product(mat1, vec1) = " + toString(product(mat1, vec2)))
  } catch {
    case e: Exception => println(e)
  }

}

/*
output:
vec1 = [5.0 2.0 6.0]
mat1 =
[1.0 2.0 3.0]
[4.0 5.0 6.0]
[7.0 8.0 9.0]

mat2 =
[10.0 11.0 12.0]
[13.0 14.0 15.0]
[16.0 17.0 18.0]

dim(mat1) = (3,3)
trace(mat1) = 15.0
transpose(mat2) =
[10.0 13.0 16.0]
[11.0 14.0 17.0]
[12.0 15.0 18.0]

sum(mat1, mat2) =
[11.0 13.0 15.0]
[17.0 19.0 21.0]
[23.0 25.0 27.0]

product(mat1, vec1) = [27.0 66.0 105.0]
java.lang.Exception: Dimensions don't match
*/
