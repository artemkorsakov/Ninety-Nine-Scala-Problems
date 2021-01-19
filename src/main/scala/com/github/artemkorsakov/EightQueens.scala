package com.github.artemkorsakov

import scala.annotation.tailrec

object EightQueens {
  def eightQueens: List[List[Int]] = {
    @tailrec
    def validDiagonals(qs: List[Int], upper: Int, lower: Int): Boolean = qs match {
      case Nil       => true
      case q :: tail => q != upper && q != lower && validDiagonals(tail, upper + 1, lower - 1)
    }
    def valid(qs: List[Int]): Boolean = qs match {
      case Nil       => true
      case q :: tail => validDiagonals(tail, q + 1, q - 1)
    }
    def eightQueensR(curQueens: List[Int], remainingCols: Set[Int]): List[List[Int]] =
      if (!valid(curQueens)) Nil
      else if (remainingCols.isEmpty) List(curQueens)
      else remainingCols.toList.flatMap(c => eightQueensR(c :: curQueens, remainingCols - c))
    eightQueensR(Nil, Set() ++ (1 to 8))
  }

}
