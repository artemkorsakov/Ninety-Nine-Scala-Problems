package com.github.artemkorsakov

object AnArithmeticPuzzle {
  def allVariants(numbers: IndexedSeq[Int]): IndexedSeq[String] =
    numbers.indices.flatMap { i =>
      val sublist  = numbers.take(i) ++ numbers.drop(i + 1)
      val variants = allVariants(s"+${numbers(i)}", sublist) ++ allVariants(s"-${numbers(i)}", sublist)
      variants.filter(solve(_) == 0).map(addBrackets)
    }

  def solve(equation: String): Int = {
    var result            = 0
    var previousOperation = '+'
    var number            = "0"
    equation.foreach { symbol =>
      if (symbol.isDigit) {
        number += symbol
      } else {
        result = applyOperation(previousOperation, result, number.toInt)
        previousOperation = symbol
        number = "0"
      }
    }
    applyOperation(previousOperation, result, number.toInt)
  }

  private def applyOperation(operation: Char, result: Int, number: Int): Int = operation match {
    case '+' => result + number
    case '-' => result - number
    case '*' => result * number
    case '/' => result / number
  }

  private def allVariants(equation: String, numbers: IndexedSeq[Int]): IndexedSeq[String] =
    if (numbers.isEmpty) {
      IndexedSeq(equation)
    } else {
      numbers.indices.flatMap { i =>
        val sublist = numbers.take(i) ++ numbers.drop(i + 1)
        val number  = numbers(i)
        val variants =
          allVariants(s"$equation+$number", sublist) ++ allVariants(s"$equation-$number", sublist) ++ allVariants(
            s"$equation*$number",
            sublist
          )
        if (solve(equation) % number == 0 && number != 0) {
          variants ++ allVariants(s"$equation/$number", sublist)
        } else {
          variants
        }
      }
    }

  private def addBrackets(equation: String): String =
    (1 until equation.length).foldLeft(equation.head.toString) { (res, i) =>
      val bracket = if ("+-*/".contains(equation(i))) s"($res)" else res
      s"$bracket${equation(i)}"
    }

}
