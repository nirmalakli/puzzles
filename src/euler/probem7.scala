package euler

object probem7 {

  def main(args: Array[String]) = {
    
    val primes = loop(2,10001,List())
    primes.foreach(println)
    println("Answer = " + primes.head)
  }

  def loop(i: Int, n: Int, primes: List[Int]): List[Int] = {

    if (n == 0)
      primes
    else if (checkPrime(i, primes))
      loop(i + 1, n - 1, i :: primes)
    else
      loop(i + 1, n, primes)
  }

  def checkPrime(i: Int, primes: List[Int]): Boolean = {
    primes filter (i % _ == 0) isEmpty
  }
}