package euler

import java.math.BigInteger

object probem3 {
  def main(args: Array[String]) = {

    val no: BigInt = new BigInt(new BigInteger("600851475143"))

    println("Result = " + findLargestPrimeFactor(no))
  }

  def findLargestPrimeFactor(no: BigInt): BigInt = {

    println("newMain(" + no + ")")

    def loop(i: BigInt): BigInt = {
      if (no % i == 0 && isPrime(i)) {
        println("Prime Factor found " + i)
        i max findLargestPrimeFactor(no / i)
      } else if (i < no) {
        loop(i + 1)
      } else {
        no
      }
    }

    if (no <= 2) {
      no
    } else {
      loop(2)
    }
  }

  def isPrime(n: BigInt): Boolean = {

    def loop(i: BigInt): Boolean = {
      if (i > n / 2 || n % i == 0)
        false
      else if (i == n / 2)
        true
      else
        loop(i + 1)
    }

    loop(2)
  }

}