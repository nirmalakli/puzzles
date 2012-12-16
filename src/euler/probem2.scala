package euler

object probem2 {
  def main(args: Array[String]) = {
    
    val stop = 4e6.toInt
    
    def fibonacci(a: Int, b: Int, accumulated: List[Int]): List[Int] = {
      if (a == 0 || b == 0) {
        fibonacci(1, 2, List[Int](2, 1))
      } else {
        val c = a + b
        if (c >= stop) {
          accumulated
        } else {
          fibonacci(b, c, c :: accumulated)
        }
      }
    }
    
    val result = fibonacci(0, 0, null).filter(_ % 2 == 0).reduceLeft(_ + _)
    println(result)
  }

}