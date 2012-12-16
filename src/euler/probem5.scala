package euler

object probem5 {

  def main(args: Array[String]) = {

    val factors = findFactors(2, List())
    println("Answer = " + factors.reduceLeft(_ * _))
  }

  def findFactors(i: Int, list: List[Int]): List[Int] = {
    if (i > 20) {
      list
    } else {
      val factors = primeFactors(i)
      val newFactorsComingFromI = factors diff list
      findFactors(i + 1, list ++ newFactorsComingFromI)
    }
  }

  // Input = 24 => Output=List(2,2,2,3)
  def primeFactors(n: Int): List[Int] = {

    def loop(i: Int, list: List[Int]): List[Int] = {
      if (i > n) {
        list
      } else if (n % i == 0 && isPrime(i)) {
        i :: list ++ primeFactors(n / i)
      } else {
        loop(i + 1, list)
      }
    }

    loop(2, List())
  }

  def isPrime(n: Int): Boolean = {

    def loop(i: Int): Boolean = {
      if (i > n / 2 || n % i == 0)
        false
      else if (i == n / 2)
        true
      else
        loop(i + 1)
    }

    if (n <= 3)
      true
    else
      loop(2)
  }

}