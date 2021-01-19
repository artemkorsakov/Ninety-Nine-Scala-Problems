package com.github.artemkorsakov

import com.github.artemkorsakov.AnArithmeticPuzzle.{ allVariants, solve }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class MiscellaneousProblemsTests extends AnyFlatSpec {
  "P90" should "Eight queens problem" in {
    println(EightQueens.eightQueens)
  }

  "P91" should "Knight's tour." in {
    val knightsTour = KnightsTour.solution
    println("count = " + knightsTour.countEnds)
    println("is = " + knightsTour.isCandidateError)

    KnightsTour.printNextTour(knightsTour)
  }

  "P92" should "Von Koch's conjecture." in {
    ???
  }

  "P93" should "An arithmetic puzzle." in {
    solve("+2-3+5+7-11") shouldBe 0
    solve("+3*5+7/11-2") shouldBe 0

    val variants = allVariants(IndexedSeq(2, 3, 5, 7, 11))
    println(variants.length)
    variants.foreach(println)
  }

  "P94" should "Generate K-regular simple graphs with N nodes." in {
    ???
  }

  "P98" should "Nonograms." in {
    ???
  }

  "P99" should "Crossword puzzle." in {
    ???
  }
}
