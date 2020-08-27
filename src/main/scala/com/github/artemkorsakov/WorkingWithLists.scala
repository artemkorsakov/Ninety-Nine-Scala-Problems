package com.github.artemkorsakov

import scala.annotation.tailrec
import scala.util.Random

object WorkingWithLists {
  val random = new Random()

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

  def duplicate[T](ls: List[T]): List[T] = ls.flatMap(i => List(i, i))

  def duplicateN[T](n: Int, ls: List[T]): List[T] = ls.flatMap(i => List.fill(n)(i))

  def drop[T](n: Int, ls: List[T]): List[T] = ls.zipWithIndex.filter(t => t._2 % n != n - 1).map(t => t._1)

  def split[T](n: Int, ls: List[T]): (List[T], List[T]) =
    (n, ls) match {
      case (_, Nil)         => (Nil, Nil)
      case (i, _) if i <= 0 => (Nil, ls)
      case (i, h :: tail) =>
        val t = split(i - 1, tail)
        (h :: t._1, t._2)
    }

  def slice[T](start: Int, end: Int, ls: List[T]): List[T] =
    (start, end, ls) match {
      case (_, _, Nil)                 => Nil
      case (_, e, _) if e <= 0         => Nil
      case (s, e, h :: tail) if s <= 0 => h :: slice(0, e - 1, tail)
      case (s, e, _ :: tail)           => slice(s - 1, e - 1, tail)
    }

  @tailrec
  def rotate[T](pos: Int, ls: List[T]): List[T] =
    (pos, ls) match {
      case (0, _)                  => ls
      case (_, Nil)                => ls
      case (_, _ :: Nil)           => ls
      case (p, _ :: _) if p < 0    => rotate(p + 1, ls.last +: ls.dropRight(1))
      case (p, h :: tail) if p > 0 => rotate(p - 1, tail :+ h)
    }

  def removeAt[T](pos: Int, ls: List[T]): (List[T], T) =
    (pos, ls) match {
      case (0, h :: tail) => (tail, h)
      case (p, h :: tail) if p > 0 =>
        val t = removeAt(p - 1, tail)
        (h +: t._1, t._2)
      case _ => throw new IllegalArgumentException
    }

  def insertAt[T](el: T, idx: Int, ls: List[T]): List[T] =
    (idx, ls) match {
      case (0, _)                  => el +: ls
      case (i, h :: tail) if i > 0 => h +: insertAt(el, idx - 1, tail)
      case _                       => throw new IllegalArgumentException
    }

  def range(startInclusive: Int, endInclusive: Int): List[Int] =
    if (endInclusive < startInclusive) {
      Nil
    } else {
      startInclusive +: range(startInclusive + 1, endInclusive)
    }

  def randomSelect[T](count: Int, ls: List[T]): List[T] =
    if (count <= 0) {
      Nil
    } else {
      val idx = random.nextInt(length(ls))
      ls(idx) +: randomSelect(count - 1, ls)
    }

  def lotto(count: Int, limitInclusive: Int): List[Int] =
    if (count <= 0) {
      Nil
    } else {
      val num = random.nextInt(limitInclusive)
      num +: lotto(count - 1, limitInclusive)
    }

  def randomPermute[T](list: List[T]): List[T] =
    if (list.isEmpty) {
      Nil
    } else {
      val idx   = random.nextInt(length(list))
      val tuple = removeAt(idx, list)
      tuple._2 +: randomPermute(tuple._1)
    }

  def combinations[T](k: Int, list: List[T]): List[List[T]] =
    if (k <= 0 || k > length(list)) {
      Nil
    } else if (k == 1) {
      list.map(List(_))
    } else if (k == length(list)) {
      List(list)
    } else {
      val withHead    = combinations(k - 1, list.tail).map(ls => list.head +: ls)
      val withoutHead = combinations(k, list.tail)
      withHead ++ withoutHead
    }

  def group[T](by: List[Int], values: List[T]): List[List[List[T]]] =
    if (by.isEmpty || by.sum > length(values)) {
      Nil
    } else {
      val head = combinations[T](by.head, values)
      if (by.length == 1) {
        List(head)
      } else {
        head.flatMap { headEl =>
          val restValues = values.filterNot(el => headEl.contains(el))
          val restGroup  = group(by.tail, restValues)
          restGroup.map(restVariant => headEl +: restVariant)
        }
      }
    }

  def lsort[T](list: List[List[T]]): List[List[T]] =
    list.sortBy(_.length)

  def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
    val lengths = list.map(_.length).groupBy(x => x).view.mapValues(_.length)
    list.sortWith((ls1, ls2) => lengths(ls1.length) - lengths(ls2.length) < 0)
  }

}
