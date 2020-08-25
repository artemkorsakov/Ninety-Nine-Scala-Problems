package com.github.artemkorsakov

import scala.annotation.tailrec

object WorkingWithLists {
  @tailrec
  def last[T](list: List[T]): T = list match {
    case h :: Nil  => h
    case _ :: tail => last(tail)
    case _         => throw new NoSuchElementException
  }

  @tailrec
  def penultimate[T](list: List[T]): T = list match {
    case h :: _ :: Nil => h
    case _ :: tail     => penultimate(tail)
    case _             => throw new NoSuchElementException
  }

}
