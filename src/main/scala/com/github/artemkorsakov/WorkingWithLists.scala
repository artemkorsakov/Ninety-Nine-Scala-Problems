package com.github.artemkorsakov

import scala.annotation.tailrec
import scala.util.Random

object WorkingWithLists {
  val random = new Random()

  /**
    * Find the last element of a list.
    */
  @tailrec
  def last[T](list: List[T]): T = list match {
    case h :: Nil  => h
    case _ :: tail => last(tail)
    case _         => throw new NoSuchElementException
  }

  /**
    * Find the last but one element of a list.
    */
  @tailrec
  def penultimate[T](list: List[T]): T = list match {
    case h :: _ :: Nil => h
    case _ :: tail     => penultimate(tail)
    case _             => throw new NoSuchElementException
  }

  /**
    * Find the Kth element of a list.
    */
  @tailrec
  def nth[T](idx: Int, list: List[T]): T =
    if (idx < 0 || idx >= list.length) {
      throw new NoSuchElementException
    } else if (idx == 0) {
      list.head
    } else {
      nth(idx - 1, list.tail)
    }

  /**
    * Find the number of elements of a list.
    */
  def length[T](list: List[T]): Int = list match {
    case Nil       => 0
    case _ :: tail => length(tail) + 1
  }

  /**
    * Reverse a list.
    */
  def reverse[T](list: List[T]): List[T] = list match {
    case h :: Nil  => List(h)
    case h :: tail => reverse(tail) :+ h
  }

  /**
    * Find out whether a list is a palindrome.
    */
  @tailrec
  def isPalindrome[T](list: List[T]): Boolean = list match {
    case Nil       => true
    case _ :: Nil  => true
    case h :: tail => h.equals(last(tail)) && isPalindrome(tail.dropRight(1))
  }

  /**
    * Flatten a nested list structure.
    */
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e           => List(e)
  }

  /**
    * Eliminate consecutive duplicates of list elements.
    * If a list contains repeated elements they should be replaced with a single copy of the element.
    * The order of the elements should not be changed.
    */
  def compress[T](ls: List[T]): List[T] = ls.foldLeft(List.empty[T]) { (res, item) =>
    if (res.lastOption.contains(item)) {
      res
    } else {
      res :+ item
    }
  }

  /**
    * Pack consecutive duplicates of list elements into sublists.
    * If a list contains repeated elements they should be placed in separate sublists.
    */
  def pack[T](ls: List[T]): List[List[T]] = ls.foldLeft(List.empty[List[T]]) { (res, item) =>
    if (res.lastOption.getOrElse(List.empty[T]).contains(item)) {
      res.dropRight(1) ++ List(res.last ++ List(item))
    } else {
      res ++ List(List(item))
    }
  }

  /**
    * Run-length encoding of a list.
    * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
    */
  def encode[T](ls: List[T]): List[(Int, T)] = ls.foldLeft(List.empty[(Int, T)]) { (res, item) =>
    if (res.nonEmpty && res.last._2.equals(item)) {
      res.dropRight(1) :+ (res.last._1 + 1, item)
    } else {
      res :+ (1, item)
    }
  }

  /**
    * Modified run-length encoding.
    */
  def encodeModified(ls: List[Any]): List[Any] = encode(ls).map(t => if (t._1 == 1) t._2 else t)

  /**
    * Decode a run-length encoded list.
    */
  def decode[T](ls: List[(Int, T)]): List[T] = ls.flatMap(t => List.fill(t._1)(t._2))

  def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { _ == ls.head }
      (packed.length, packed.head) :: encodeDirect(next)
    }

  /**
    * Duplicate the elements of a list.
    */
  def duplicate[T](ls: List[T]): List[T] = ls.flatMap(i => List(i, i))

  /**
    * Duplicate the elements of a list a given number of times.
    */
  def duplicateN[T](n: Int, ls: List[T]): List[T] = ls.flatMap(i => List.fill(n)(i))

  /**
    * Drop every Nth element from a list.
    */
  def drop[T](n: Int, ls: List[T]): List[T] = ls.zipWithIndex.filter(t => t._2 % n != n - 1).map(t => t._1)

  /**
    * Split a list into two parts.
    * The length of the first part is given. Use a Tuple for your result.
    */
  def split[T](n: Int, ls: List[T]): (List[T], List[T]) =
    (n, ls) match {
      case (_, Nil)         => (Nil, Nil)
      case (i, _) if i <= 0 => (Nil, ls)
      case (i, h :: tail) =>
        val t = split(i - 1, tail)
        (h :: t._1, t._2)
    }

  /**
    * Extract a slice from a list.
    * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to
    * but not including the Kth element of the original list. Start counting the elements with 0.
    */
  def slice[T](start: Int, end: Int, ls: List[T]): List[T] =
    (start, end, ls) match {
      case (_, _, Nil)                 => Nil
      case (_, e, _) if e <= 0         => Nil
      case (s, e, h :: tail) if s <= 0 => h :: slice(0, e - 1, tail)
      case (s, e, _ :: tail)           => slice(s - 1, e - 1, tail)
    }

  // Rotate a list N places to the left.
  @tailrec
  def rotate[T](pos: Int, ls: List[T]): List[T] =
    (pos, ls) match {
      case (0, _)                  => ls
      case (_, Nil)                => ls
      case (_, _ :: Nil)           => ls
      case (p, _ :: _) if p < 0    => rotate(p + 1, ls.last +: ls.dropRight(1))
      case (p, h :: tail) if p > 0 => rotate(p - 1, tail :+ h)
    }

  // Remove the Kth element from a list.
  def removeAt[T](pos: Int, ls: List[T]): (List[T], T) =
    (pos, ls) match {
      case (0, h :: tail) => (tail, h)
      case (p, h :: tail) if p > 0 =>
        val t = removeAt(p - 1, tail)
        (h +: t._1, t._2)
      case _ => throw new IllegalArgumentException
    }

  // Insert an element at a given position into a list.
  def insertAt[T](el: T, idx: Int, ls: List[T]): List[T] =
    (idx, ls) match {
      case (0, _)                  => el +: ls
      case (i, h :: tail) if i > 0 => h +: insertAt(el, idx - 1, tail)
      case _                       => throw new IllegalArgumentException
    }

  // Create a list containing all integers within a given range.
  def range(startInclusive: Int, endInclusive: Int): List[Int] =
    if (endInclusive < startInclusive) {
      Nil
    } else {
      startInclusive +: range(startInclusive + 1, endInclusive)
    }

  // Extract a given number of randomly selected elements from a list.
  def randomSelect[T](count: Int, ls: List[T]): List[T] =
    if (count <= 0) {
      Nil
    } else {
      val idx = random.nextInt(length(ls))
      ls(idx) +: randomSelect(count - 1, ls)
    }

  // Lotto: Draw N different random numbers from the set 1..M.
  def lotto(count: Int, limitInclusive: Int): List[Int] =
    if (count <= 0) {
      Nil
    } else {
      val num = random.nextInt(limitInclusive)
      num +: lotto(count - 1, limitInclusive)
    }

  // Generate a random permutation of the elements of a list.
  def randomPermute[T](list: List[T]): List[T] =
    if (list.isEmpty) {
      Nil
    } else {
      val idx   = random.nextInt(length(list))
      val tuple = removeAt(idx, list)
      tuple._2 +: randomPermute(tuple._1)
    }

  /**
    * Generate the combinations of K distinct objects chosen from the N elements of a list.
    * In how many ways can a committee of 3 be chosen from a group of 12 people?
    * We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
    * For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
    */
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

  // Group the elements of a set into disjoint subsets.
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

  // Sorting a list of lists according to length of sublists.
  def lsort[T](list: List[List[T]]): List[List[T]] =
    list.sortBy(_.length)

  // Sorting a list of lists according to length of sublists.
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
    val lengths = list.map(_.length).groupBy(x => x).view.mapValues(_.length)
    list.sortWith((ls1, ls2) => lengths(ls1.length) - lengths(ls2.length) < 0)
  }

}
