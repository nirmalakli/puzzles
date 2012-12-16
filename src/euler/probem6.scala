package euler

object probem6 {

  def main(args: Array[String]) = {
    
    val nos = 1 until 101
    val sumOfSquares = nos map (x=> x*x) reduceLeft(_ + _)
    val sum = nos reduceLeft(_ + _)
    val squareOfSum = sum * sum
    
    println("Answer = " + (squareOfSum - sumOfSquares) )
  }
}