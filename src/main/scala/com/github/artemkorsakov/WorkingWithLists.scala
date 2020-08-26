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

  @tailrec
  def nth[T](idx: Int, list: List[T]): T =
    if (idx < 0 || idx >= list.length) {
      throw new NoSuchElementException
    } else if (idx == 0) {
      list.head
    } else {
      nth(idx - 1, list.tail)
    }

  def length[T](list: List[T]): Int = list match {
    case Nil       => 0
    case _ :: tail => length(tail) + 1
  }

  def reverse[T](list: List[T]): List[T] = list match {
    case h :: Nil  => List(h)
    case h :: tail => reverse(tail) :+ h
  }

  @tailrec
  def isPalindrome[T](list: List[T]): Boolean = list match {
    case Nil       => true
    case _ :: Nil  => true
    case h :: tail => h.equals(last(tail)) && isPalindrome(tail.dropRight(1))
  }

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e           => List(e)
  }

  def compress[T](ls: List[T]): List[T] = ls.foldLeft(List.empty[T]) { (res, item) =>
    if (res.lastOption.contains(item)) {
      res
    } else {
      res :+ item
    }
  }

  def pack[T](ls: List[T]): List[List[T]] = ls.foldLeft(List.empty[List[T]]) { (res, item) =>
    if (res.lastOption.getOrElse(List.empty[T]).contains(item)) {
      res.dropRight(1) ++ List(res.last ++ List(item))
    } else {
      res ++ List(List(item))
    }
  }

  def encode[T](ls: List[T]): List[(Int, T)] = ls.foldLeft(List.empty[(Int, T)]) { (res, item) =>
    if (res.nonEmpty && res.last._2.equals(item)) {
      res.dropRight(1) :+ (res.last._1 + 1, item)
    } else {
      res :+ (1, item)
    }
  }

  def encodeModified(ls: List[Any]): List[Any] = encode(ls).map(t => if (t._1 == 1) t._2 else t)

  def decode[T](ls: List[(Int, T)]): List[T] = ls.flatMap(t => List.fill(t._1)(t._2))

  def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { _ == ls.head }
      (packed.length, packed.head) :: encodeDirect(next)
    }
}
