package com.github.artemkorsakov

import com.github.artemkorsakov.KnightPosition._

case class KnightPosition(x: Int, y: Int) {
  require(isCorrect(x, y), "Invalid position")

  def nextPositions: Vector[KnightPosition] =
    Vector(
      (x - 1, y + 2),
      (x + 1, y + 2),
      (x - 1, y - 2),
      (x + 1, y - 2),
      (x - 2, y - 1),
      (x - 2, y + 1),
      (x + 2, y - 1),
      (x + 2, y + 1)
    )
      .withFilter { case (x, y) => isCorrect(x, y) }
      .map { case (x, y) => KnightPosition(x, y) }
}

object KnightPosition {
  def isCorrect(x: Int, y: Int): Boolean = 0 <= x && x < 8 && 0 <= y && y < 8
}

class KnightsTour(
    val currentPosition: KnightPosition = KnightPosition(0, 0),
    val steps: Int = 1,
    val knownPositions: Array[Array[Int]] = (0 until 8).map(_ => (0 until 8).map(_ => 0).toArray).toArray
) {
  knownPositions(currentPosition.x)(currentPosition.y) = steps

  def nextTours: Vector[KnightsTour] =
    currentPosition.nextPositions
      .withFilter(pos => knownPositions(pos.x)(pos.y) == 0)
      .map { nextPosition =>
        val newKnownPositions: Array[Array[Int]] = new Array[Array[Int]](8)
        (0 until 8).foreach { r =>
          newKnownPositions(r) = new Array[Int](8)
          knownPositions(r).copyToArray(newKnownPositions(r))
        }
        new KnightsTour(nextPosition, steps + 1, newKnownPositions)
      }

  def isNotFill: Boolean = steps < 55

  def isCandidateError: Boolean = {
    val countE = countEnds
    val countZ = countZeros
    countE > 1 || (countE == 1 && countZ > 1)
  }

  def countEnds: Int =
    knownPositions.indices
      .map(x =>
        knownPositions(x).indices.count(y =>
          knownPositions(x)(y) == 0 &&
          KnightPosition(x, y).nextPositions.forall(kp => knownPositions(kp.x)(kp.y) > 0)
        )
      )
      .sum

  def countZeros: Int =
    knownPositions.indices.map(x => knownPositions(x).indices.count(knownPositions(x)(_) == 0)).sum
}

object KnightsTour {
  def solution: KnightsTour = {
    val knightsTour   = new KnightsTour()
    val nextToursList = knightsTour.nextTours
    var candidate     = nextToursList.head
    var others        = nextToursList.tail

    while (candidate.isNotFill) {
      val candidateNextTours = candidate.nextTours.filterNot(_.isCandidateError)
      if (candidateNextTours.isEmpty) {
        candidate = others.head
        others = others.tail
      } else {
        candidate = candidateNextTours.head
        others = candidateNextTours.tail ++ others
      }
    }

    candidate
  }

  def printNextTour(nextTours: List[KnightsTour]): Unit =
    nextTours.foreach(printNextTour)

  def printNextTour(nextTour: KnightsTour): Unit = {
    println("Test start")
    println(nextTour.currentPosition)
    println(nextTour.steps)
    nextTour.knownPositions.foreach(row => println(row.mkString("Array(", ", ", ")")))
    println("Test finish")
  }
}
