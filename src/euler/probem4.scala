package euler

object probem4 {
  
  def main(args: Array[String]) : Unit = {

    // Scope for optimization.
    val nos = for {
      i <- 1 until 1000
      j <- 1 until 1000
    } yield i * j
    
    val palindromes = nos filter isPalindrome sortWith( _ > _)
    println(palindromes head)
  }
  
  def isPalindrome(i:Int) : Boolean = {
    val str = i.toString
    str == str.reverse // Scope for optimization
  }
}