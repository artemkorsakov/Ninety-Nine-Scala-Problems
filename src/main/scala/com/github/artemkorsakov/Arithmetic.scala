package com.github.artemkorsakov {
  import com.github.artemkorsakov.Arithmetic._

  import scala.annotation.tailrec
  import scala.collection.mutable
  class Arithmetic(n: Int) {
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

    def isCoprimeTo(m: Int): Boolean = gcd(n, m) == 1

    /**
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

    def primeFactorMultiplicity: mutable.Map[Int, Int] = getAllPrimeFactorsWithPow(n)

    def primeFactors: List[Int] = primeFactorMultiplicity.flatMap(item => List.fill(item._2)(item._1)).toList

    def goldbach: (Int, Int) = {
      val p = (2 to n / 2).find(p => p.isPrime && (n - p).isPrime).getOrElse(0)
      (p, n - p)
    }
  }

  object Arithmetic {
    implicit def int2ArithmeticInt(i: Int): Arithmetic = new Arithmetic(i)

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

    def getAllPrimesNotMoreThanLimit(limit: Int): Array[Int] =
      getAllPrimesNotMoreThanLimitInBooleanArray(limit).zipWithIndex.filter(_._1).map(_._2)

    def listPrimesInRange(range: Range): List[Int] = range.filter(_.isPrime).toList

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

    def goldbachList(range: Range): Map[Int, (Int, Int)] =
      range.filter(i => i > 2 && i % 2 == 0).map(i => i -> i.goldbach).toMap

    def goldbachListLimited(range: Range, min: Int): Map[Int, (Int, Int)] =
      goldbachList(range).filter(item => item._2._1 >= min)

  }

}
