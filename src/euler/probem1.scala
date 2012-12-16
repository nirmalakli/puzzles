package euler

object probem1 {
  def main(args: Array[String]) = {
    val numbers = 1 to 999
    val result = numbers filter (x => x % 3 == 0 || x % 5 == 0) reduceLeft (_ + _)
    println(result)
  }
}