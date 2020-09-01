package com.github.artemkorsakov {
  import com.github.artemkorsakov.Arithmetic._

  import scala.annotation.tailrec
  import scala.collection.mutable
  class Arithmetic(n: Int) {

    /**
      * Determine whether a given integer number is prime.
      */
    def isPrime: Boolean =
      if (n < 2) {
        false
      } else if (n < 4) {
        true
      } else if (n % 2 == 0) {
        false
      } else if (n < 9) {
        true
      } else if (n % 3 == 0) {
        false
      } else {
        val sqrt      = math.sqrt(n).toLong
        var candidate = 5
        while (candidate <= sqrt && n % candidate != 0) {
          candidate += (if (candidate % 6 == 5) 2 else 4)
        }
        n % candidate != 0
      }

    /**
      * Determine whether two positive integer numbers are coprime.
      * Two numbers are coprime if their greatest common divisor equals 1.
      */
    def isCoprimeTo(m: Int): Boolean = gcd(n, m) == 1

    /**
      * Calculate Euler's totient function phi(m).
      * Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
      * Counts the positive integers up to a given integer n that are relatively prime to n.
      *
      * @see <a href="https://en.wikipedia.org/wiki/Euler%27s_totient_function">detailed description</a>
      */
    def totient: Int = {
      var result: Double = n
      val keys           = getAllPrimeFactorsWithPow(n).keySet
      for (prime <- keys) {
        result *= (1.0 - 1.0 / prime)
      }
      result.toInt
    }

    /**
      * Determine the prime factors of a given positive integer (2).
      * Construct a list containing the prime factors and their multiplicity.
      */
    def primeFactorMultiplicity: mutable.Map[Int, Int] = getAllPrimeFactorsWithPow(n)

    /**
      * Determine the prime factors of a given positive integer.
      * Construct a flat list containing the prime factors in ascending order.
      */
    def primeFactors: List[Int] = primeFactorMultiplicity.flatMap(item => List.fill(item._2)(item._1)).toList

    /**
      * Goldbach's conjecture.
      * Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
      * E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
      * It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent).
      * Write a function to find the two prime numbers that sum up to a given even integer.
      */
    def goldbach: (Int, Int) = {
      val p = (2 to n / 2).find(p => p.isPrime && (n - p).isPrime).getOrElse(0)
      (p, n - p)
    }
  }

  object Arithmetic {
    implicit def int2ArithmeticInt(i: Int): Arithmetic = new Arithmetic(i)

    /**
      * A list of prime numbers.
      * Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
      */
    def getAllPrimesNotMoreThanLimitInBooleanArray(limit: Int): Array[Boolean] = {
      val result: Array[Boolean] = Array.fill(limit + 1)(false)
      result(2) = true
      result(3) = true

      var candidate = 5
      while (candidate <= limit) {
        result(candidate) = true
        candidate += 2
        if (candidate <= limit) {
          result(candidate) = true
        }
        candidate += 4
      }

      candidate = 5
      while (candidate * candidate <= limit) {
        if (result(candidate)) {
          var j = candidate * candidate
          while (j <= limit) {
            result(j) = false
            j += candidate * 2
          }
        }
        candidate += 2
        if (candidate <= limit && result(candidate)) {
          var j = candidate * candidate
          while (j <= limit) {
            result(j) = false
            j += candidate * 2
          }
        }
        candidate += 4
      }

      result
    }

    /**
      * A list of prime numbers.
      * Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
      */
    def getAllPrimesNotMoreThanLimit(limit: Int): Array[Int] =
      getAllPrimesNotMoreThanLimitInBooleanArray(limit).zipWithIndex.filter(_._1).map(_._2)

    /**
      * A list of prime numbers.
      * Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
      */
    def listPrimesInRange(range: Range): List[Int] = range.filter(_.isPrime).toList

    /**
      * Determine the greatest common divisor of two positive integer numbers.
      */
    @tailrec
    def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

    /**
      * Get all prime divisors of a number along with its powers.
      */
    def getAllPrimeFactorsWithPow(number: Long): mutable.Map[Int, Int] = {
      val result = mutable.Map.empty[Int, Int]

      var i = 2
      var n = number
      while (n > 1) {
        if (i.isPrime && n % i == 0) {
          var pow = 0
          while (n % i == 0) {
            n /= i
            pow += 1
          }
          result += (i -> pow)
        }
        i += 1
      }

      result
    }

    /**
      * A list of Goldbach compositions.
      * Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
      */
    def goldbachList(range: Range): Map[Int, (Int, Int)] =
      range.filter(i => i > 2 && i % 2 == 0).map(i => i -> i.goldbach).toMap

    /**
      * In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than, say, 50. Try to find out how many such cases there are in the range 2..3000.
      *
      * Example (minimum value of 50 for the primes):
      *
      * scala> printGoldbachListLimited(1 to 2000, 50)
      * 992 = 73 + 919
      * 1382 = 61 + 1321
      * 1856 = 67 + 1789
      * 1928 = 61 + 1867
      */
    def goldbachListLimited(range: Range, min: Int): Map[Int, (Int, Int)] =
      goldbachList(range).filter(item => item._2._1 >= min)

  }

}
