package com.github.artemkorsakov

import com.github.artemkorsakov.Arithmetic._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable

class ArithmeticTests extends AnyFlatSpec {
  "Primes array" should "return correct results" in {
    val array = getAllPrimesNotMoreThanLimitInBooleanArray(100)
    array.zipWithIndex.forall(tuple => tuple._2.isPrime == tuple._1)
    println(getAllPrimesNotMoreThanLimit(100).mkString("Array(", ", ", ")"))
    getAllPrimeFactorsWithPow(300) shouldBe mutable.HashMap(2 -> 2, 3 -> 1, 5 -> 2)
  }

  "P31" should "Determine whether a given integer number is prime." in {
    7.isPrime shouldBe true
  }

  "P32" should "Determine the greatest common divisor of two positive integer numbers." in {
    gcd(36, 63) shouldBe 9
  }

  "P33" should "Determine whether two positive integer numbers are coprime." in {
    35.isCoprimeTo(64) shouldBe true
  }

  "P34" should "Calculate Euler's totient function phi(m)." in {
    10.totient shouldBe 4
  }

  "P35" should "Determine the prime factors of a given positive integer." in {
    315.primeFactors shouldBe List(3, 3, 5, 7)
  }

  "P36" should "Determine the prime factors of a given positive integer (2)." in {
    315.primeFactorMultiplicity shouldBe mutable.HashMap(3 -> 2, 5 -> 1, 7 -> 1)
  }

  "P37" should "Calculate Euler's totient function phi(m) (improved)." in {
    1000000.totient shouldBe 400000
  }

  "P38" should "Determine whether a given integer number is prime." in {
    10090.totient shouldBe 4032
  }

  "P39" should "A list of prime numbers." in {
    listPrimesInRange(7 to 31) shouldBe List(7, 11, 13, 17, 19, 23, 29, 31)
  }

  "P40" should "Goldbach's conjecture." in {
    28.goldbach shouldBe (5, 23)
  }

  "P41" should "A list of Goldbach compositions." in {
    goldbachList(9 to 20) shouldBe mutable.HashMap(
      10 -> (3, 7),
      12 -> (5, 7),
      14 -> (3, 11),
      16 -> (3, 13),
      18 -> (5, 13),
      20 -> (3, 17)
    )

    goldbachListLimited(1 to 2000, 50) shouldBe mutable.HashMap(
      992  -> (73, 919),
      1382 -> (61, 1321),
      1856 -> (67, 1789),
      1928 -> (61, 1867)
    )
  }

}
