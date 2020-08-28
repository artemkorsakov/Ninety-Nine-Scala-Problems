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

}
