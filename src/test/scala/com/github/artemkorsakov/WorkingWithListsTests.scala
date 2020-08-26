package com.github.artemkorsakov

import com.github.artemkorsakov.WorkingWithLists._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class WorkingWithListsTests extends AnyFlatSpec {
  "P01 (*)" should "find the last element of a list." in {
    last(List(8)) shouldBe 8
    last(List(1, 1, 2, 3, 5, 8)) shouldBe 8
    assertThrows[NoSuchElementException] {
      last(List())
    }
  }

  "P02 (*)" should "find the last but one element of a list." in {
    penultimate(List(5, 8)) shouldBe 5
    penultimate(List(1, 1, 2, 3, 5, 8)) shouldBe 5
    assertThrows[NoSuchElementException] {
      penultimate(List())
    }
    assertThrows[NoSuchElementException] {
      penultimate(List(8))
    }
  }

  "P03 (*)" should "find the Kth element of a list." in {
    nth(2, List(1, 1, 2, 3, 5, 8)) shouldBe 2
    nth(0, List(1, 1, 2, 3, 5, 8)) shouldBe 1
    nth(5, List(1, 1, 2, 3, 5, 8)) shouldBe 8
    nth(4, List(1, 1, 2, 3, 5, 8)) shouldBe 5
    assertThrows[NoSuchElementException] {
      nth(-1, List(1, 1, 2, 3, 5, 8))
    }
    assertThrows[NoSuchElementException] {
      nth(6, List(1, 1, 2, 3, 5, 8))
    }
  }

  "P04 (*)" should "find the number of elements of a list." in {
    WorkingWithLists.length(List(1, 1, 2, 3, 5, 8)) shouldBe 6
  }

  "P05 (*)" should "reverse a list." in {
    reverse(List(1, 1, 2, 3, 5, 8)) shouldBe List(8, 5, 3, 2, 1, 1)
  }

  "P06 (*)" should "find out whether a list is a palindrome." in {
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
  }

  "P07 (*)" should "flatten a nested list structure." in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
  }

  "P08 (*)" should "eliminate consecutive duplicates of list elements." in {
    compress(
      List(
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("a"),
        Symbol("a"),
        Symbol("d"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e")
      )
    ) shouldBe List(
      Symbol("a"),
      Symbol("b"),
      Symbol("c"),
      Symbol("a"),
      Symbol("d"),
      Symbol("e")
    )
  }

  "P09 (*)" should "pack consecutive duplicates of list elements into sublists." in {
    pack(
      List(
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("a"),
        Symbol("a"),
        Symbol("d"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e")
      )
    ) shouldBe List(
      List(Symbol("a"), Symbol("a"), Symbol("a"), Symbol("a")),
      List(Symbol("b")),
      List(Symbol("c"), Symbol("c")),
      List(Symbol("a"), Symbol("a")),
      List(Symbol("d")),
      List(Symbol("e"), Symbol("e"), Symbol("e"), Symbol("e"))
    )
  }

  "P10 (*)" should "run-length encoding of a list." in {
    encode(
      List(
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("c"),
        Symbol("a"),
        Symbol("a"),
        Symbol("d"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e"),
        Symbol("e")
      )
    ) shouldBe List(
      (4, Symbol("a")),
      (1, Symbol("b")),
      (2, Symbol("c")),
      (2, Symbol("a")),
      (1, Symbol("d")),
      (4, Symbol("e"))
    )
  }

  "P11 (*)" should "" in {}

  "P12 (*)" should "" in {}

  "P13 (*)" should "" in {}

  "P14 (*)" should "" in {}

  "P15 (*)" should "" in {}

  "P16 (*)" should "" in {}

  "P17 (*)" should "" in {}

  "P18 (*)" should "" in {}

  "P19 (*)" should "" in {}

  "P20 (*)" should "" in {}

  "P21 (*)" should "" in {}

  "P22 (*)" should "" in {}

}
