package com.github.artemkorsakov

import com.github.artemkorsakov.Logic._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class LogicTests extends AnyFlatSpec {

  "P46" should "Truth tables for logical expressions." in {
    and(a = true, b = true) shouldBe true
    and(a = true, b = false) shouldBe false
    and(a = false, b = true) shouldBe false
    and(a = false, b = false) shouldBe false
    table2(and)

    nand(a = true, b = true) shouldBe false
    nand(a = true, b = false) shouldBe true
    nand(a = false, b = true) shouldBe true
    nand(a = false, b = false) shouldBe true
    table2(nand)

    or(a = true, b = true) shouldBe true
    or(a = true, b = false) shouldBe true
    or(a = false, b = true) shouldBe true
    or(a = false, b = false) shouldBe false
    table2(or)

    nor(a = true, b = true) shouldBe false
    nor(a = true, b = false) shouldBe false
    nor(a = false, b = true) shouldBe false
    nor(a = false, b = false) shouldBe true
    table2(nor)

    xor(a = true, b = true) shouldBe false
    xor(a = true, b = false) shouldBe true
    xor(a = false, b = true) shouldBe true
    xor(a = false, b = false) shouldBe false
    table2(xor)

    impl(a = true, b = true) shouldBe true
    impl(a = true, b = false) shouldBe false
    impl(a = false, b = true) shouldBe true
    impl(a = false, b = false) shouldBe true
    table2(impl)

    equ(a = true, b = true) shouldBe true
    equ(a = true, b = false) shouldBe false
    equ(a = false, b = true) shouldBe false
    equ(a = false, b = false) shouldBe true
    table2(equ)
  }

  "P47" should "Truth tables for logical expressions (2)." in {
    true and true shouldBe true
    true and false shouldBe false
    false and true shouldBe false
    false and false shouldBe false

    true nand true shouldBe false
    true nand false shouldBe true
    false nand true shouldBe true
    false nand false shouldBe true

    true or true shouldBe true
    true or false shouldBe true
    false or true shouldBe true
    false or false shouldBe false

    true nor true shouldBe false
    true nor false shouldBe false
    false nor true shouldBe false
    false nor false shouldBe true

    true xor true shouldBe false
    true xor false shouldBe true
    false xor true shouldBe true
    false xor false shouldBe false

    true impl true shouldBe true
    true impl false shouldBe false
    false impl true shouldBe true
    false impl false shouldBe true

    true equ true shouldBe true
    true equ false shouldBe false
    false equ true shouldBe false
    false equ false shouldBe true
  }

  "P49" should "Gray code." in {
    gray(1) shouldBe Set("0", "1")
    gray(2) shouldBe Set("00", "01", "11", "10")
    gray(3) shouldBe Set("000", "001", "011", "010", "110", "111", "101", "100")
    gray(10).size shouldBe 1024
  }

  "P50" should "Huffman code." in {
    huffman(Seq(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))) shouldBe Set(
      ("a", 0),
      ("b", 101),
      ("c", 100),
      ("d", 111),
      ("e", 1101),
      ("f", 1100)
    )
  }

}
