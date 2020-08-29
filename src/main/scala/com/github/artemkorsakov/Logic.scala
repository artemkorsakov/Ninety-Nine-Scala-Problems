package com.github.artemkorsakov

class Logic(a: Boolean) {
  def not: Boolean              = Logic.not(a)
  def and(b: Boolean): Boolean  = Logic.and(a, b)
  def nand(b: Boolean): Boolean = Logic.nand(a, b)
  def or(b: Boolean): Boolean   = Logic.or(a, b)
  def nor(b: Boolean): Boolean  = Logic.nor(a, b)
  def equ(b: Boolean): Boolean  = Logic.equ(a, b)
  def xor(b: Boolean): Boolean  = Logic.xor(a, b)
  def impl(b: Boolean): Boolean = Logic.impl(a, b)
}

object Logic {
  implicit def bool2Logic(a: Boolean): Logic = new Logic(a)

  def not(a: Boolean): Boolean =
    if (a) {
      false
    } else {
      true
    }

  def and(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _            => false
  }

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

  def or(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (false, false) => false
    case _              => true
  }

  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))

  def equ(a: Boolean, b: Boolean): Boolean  = or(and(a, b), and(not(a), not(b)))
  def xor(a: Boolean, b: Boolean): Boolean  = not(equ(a, b))
  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)

  def table2(f: (Boolean, Boolean) => Boolean) {
    println("A     B     result")
    for {
      a <- List(true, false)
      b <- List(true, false)
    } {
      printf("%-5s %-5s %-5s\n", a, b, f(a, b))
    }
  }

  def gray(c: Int): Set[String] =
    if (c <= 0) {
      Set.empty[String]
    } else if (c == 1) {
      Set("0", "1")
    } else {
      val set = gray(c - 1)
      set.flatMap(str => Set("0" + str, "1" + str))
    }

  def huffman(seq: Seq[(String, Int)]): Set[(String, Int)] = {
    var tree   = Seq.from(seq)
    var result = seq.map(item => (item._1, "")).toSet

    while (tree.length > 1) {
      val min1 = tree.minBy(_._2)
      val min2 = tree.filterNot(_._1 == min1._1).minBy(_._2)
      val sum  = (min1._1 + min2._1, min1._2 + min2._2)
      tree = tree.filterNot(item => item._1 == min1._1 || item._1 == min2._1) :+ sum
      result = result.map { item =>
        if (min1._1.contains(item._1)) {
          (item._1, "0" + item._2)
        } else if (min2._1.contains(item._1)) {
          (item._1, "1" + item._2)
        } else {
          item
        }
      }
    }

    result.map(item => (item._1, item._2.toInt))
  }

}
