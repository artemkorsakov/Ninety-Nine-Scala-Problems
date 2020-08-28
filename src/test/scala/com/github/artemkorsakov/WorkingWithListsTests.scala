package com.github.artemkorsakov

import com.github.artemkorsakov.WorkingWithLists._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class WorkingWithListsTests extends AnyFlatSpec {
  "P01" should "find the last element of a list." in {
    last(List(8)) shouldBe 8
    last(List(1, 1, 2, 3, 5, 8)) shouldBe 8
    assertThrows[NoSuchElementException] {
      last(List())
    }
  }

  "P02" should "find the last but one element of a list." in {
    penultimate(List(5, 8)) shouldBe 5
    penultimate(List(1, 1, 2, 3, 5, 8)) shouldBe 5
    assertThrows[NoSuchElementException] {
      penultimate(List())
    }
    assertThrows[NoSuchElementException] {
      penultimate(List(8))
    }
  }

  "P03" should "find the Kth element of a list." in {
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

  "P04" should "find the number of elements of a list." in {
    WorkingWithLists.length(List(1, 1, 2, 3, 5, 8)) shouldBe 6
  }

  "P05" should "reverse a list." in {
    reverse(List(1, 1, 2, 3, 5, 8)) shouldBe List(8, 5, 3, 2, 1, 1)
  }

  "P06" should "find out whether a list is a palindrome." in {
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
  }

  "P07" should "flatten a nested list structure." in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
  }

  "P08" should "eliminate consecutive duplicates of list elements." in {
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

  "P09" should "pack consecutive duplicates of list elements into sublists." in {
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

  "P10" should "run-length encoding of a list." in {
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

  "P11" should "modified run-length encoding." in {
    encodeModified(
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
      Symbol("b"),
      (2, Symbol("c")),
      (2, Symbol("a")),
      Symbol("d"),
      (4, Symbol("e"))
    )
  }

  "P12" should "decode a run-length encoded list." in {
    decode(
      List((4, Symbol("a")), (1, Symbol("b")), (2, Symbol("c")), (2, Symbol("a")), (1, Symbol("d")), (4, Symbol("e")))
    ) shouldBe List(
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
  }

  "P13" should "run-length encoding of a list (direct solution)." in {
    encodeDirect(
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

  "P14" should "duplicate the elements of a list." in {
    duplicate(List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("c"), Symbol("d"))) shouldBe List(
      Symbol("a"),
      Symbol("a"),
      Symbol("b"),
      Symbol("b"),
      Symbol("c"),
      Symbol("c"),
      Symbol("c"),
      Symbol("c"),
      Symbol("d"),
      Symbol("d")
    )
  }

  "P15" should "duplicate the elements of a list a given number of times." in {
    duplicateN(3, List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("c"), Symbol("d"))) shouldBe List(
      Symbol("a"),
      Symbol("a"),
      Symbol("a"),
      Symbol("b"),
      Symbol("b"),
      Symbol("b"),
      Symbol("c"),
      Symbol("c"),
      Symbol("c"),
      Symbol("c"),
      Symbol("c"),
      Symbol("c"),
      Symbol("d"),
      Symbol("d"),
      Symbol("d")
    )
  }

  "P16" should "drop every Nth element from a list." in {
    drop(
      3,
      List(
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("d"),
        Symbol("e"),
        Symbol("f"),
        Symbol("g"),
        Symbol("h"),
        Symbol("i"),
        Symbol("j"),
        Symbol("k")
      )
    ) shouldBe List(
      Symbol("a"),
      Symbol("b"),
      Symbol("d"),
      Symbol("e"),
      Symbol("g"),
      Symbol("h"),
      Symbol("j"),
      Symbol("k")
    )
  }

  "P17" should "split a list into two parts." in {
    split(
      3,
      List(
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("d"),
        Symbol("e"),
        Symbol("f"),
        Symbol("g"),
        Symbol("h"),
        Symbol("i"),
        Symbol("j"),
        Symbol("k")
      )
    ) shouldBe (List(Symbol("a"), Symbol("b"), Symbol("c")), List(
      Symbol("d"),
      Symbol("e"),
      Symbol("f"),
      Symbol("g"),
      Symbol("h"),
      Symbol("i"),
      Symbol("j"),
      Symbol("k")
    ))
  }

  "P18" should "extract a slice from a list." in {
    slice(
      3,
      7,
      List(
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("d"),
        Symbol("e"),
        Symbol("f"),
        Symbol("g"),
        Symbol("h"),
        Symbol("i"),
        Symbol("j"),
        Symbol("k")
      )
    ) shouldBe List(Symbol("d"), Symbol("e"), Symbol("f"), Symbol("g"))
  }

  "P19" should "rotate a list N places to the left." in {
    rotate(
      3,
      List(
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("d"),
        Symbol("e"),
        Symbol("f"),
        Symbol("g"),
        Symbol("h"),
        Symbol("i"),
        Symbol("j"),
        Symbol("k")
      )
    ) shouldBe List(
      Symbol("d"),
      Symbol("e"),
      Symbol("f"),
      Symbol("g"),
      Symbol("h"),
      Symbol("i"),
      Symbol("j"),
      Symbol("k"),
      Symbol("a"),
      Symbol("b"),
      Symbol("c")
    )
    rotate(
      -2,
      List(
        Symbol("a"),
        Symbol("b"),
        Symbol("c"),
        Symbol("d"),
        Symbol("e"),
        Symbol("f"),
        Symbol("g"),
        Symbol("h"),
        Symbol("i"),
        Symbol("j"),
        Symbol("k")
      )
    ) shouldBe List(
      Symbol("j"),
      Symbol("k"),
      Symbol("a"),
      Symbol("b"),
      Symbol("c"),
      Symbol("d"),
      Symbol("e"),
      Symbol("f"),
      Symbol("g"),
      Symbol("h"),
      Symbol("i")
    )
  }

  "P20" should "remove the Kth element from a list." in {
    removeAt(1, List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d"))) shouldBe (List(
      Symbol("a"),
      Symbol("c"),
      Symbol("d")
    ), Symbol("b"))
  }

  "P21" should "insert an element at a given position into a list." in {
    insertAt(Symbol("new"), 1, List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d"))) shouldBe List(
      Symbol("a"),
      Symbol("new"),
      Symbol("b"),
      Symbol("c"),
      Symbol("d")
    )
  }

  "P22" should "create a list containing all integers within a given range." in {
    range(4, 9) shouldBe List(4, 5, 6, 7, 8, 9)
  }

  "P23" should "extract a given number of randomly selected elements from a list." in {
    val list       = List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d"), Symbol("f"), Symbol("g"), Symbol("h"))
    val randomList = randomSelect(3, list)
    randomList.length shouldBe 3
    randomList.forall(el => list.contains(el)) shouldBe true
    println(randomList)
  }

  "P24" should "Lotto: Draw N different random numbers from the set 1..M." in {
    val randomList = lotto(6, 49)
    randomList.length shouldBe 6
    randomList.forall(el => 0 <= el && el <= 49) shouldBe true
    println(randomList)
  }

  "P25" should "Generate a random permutation of the elements of a list." in {
    val list       = List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d"), Symbol("e"), Symbol("f"))
    val randomList = randomPermute(list)
    randomList.length shouldBe 6
    randomList.forall(el => list.contains(el)) shouldBe true
    println(randomList)
  }

  "P26" should "Generate the combinations of K distinct objects chosen from the N elements of a list." in {
    val list            = List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d"), Symbol("e"), Symbol("f"))
    val allCombinations = combinations(3, list)
    allCombinations.length shouldBe 20
    allCombinations.forall(_.forall(list.contains(_))) shouldBe true
    println(allCombinations)
  }

  "P27" should "Group the elements of a set into disjoint subsets." in {
    val list   = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val groups = group(List(2, 2, 5), list)
    groups.length shouldBe 756
    println(groups)
  }

  "P28" should "Sorting a list of lists according to length of sublists." in {
    lsort(
      List(
        List(Symbol("a"), Symbol("b"), Symbol("c")),
        List(Symbol("d"), Symbol("e")),
        List(Symbol("f"), Symbol("g"), Symbol("h")),
        List(Symbol("d"), Symbol("e")),
        List(Symbol("i"), Symbol("j"), Symbol("k"), Symbol("l")),
        List(Symbol("m"), Symbol("n")),
        List(Symbol("o"))
      )
    ) shouldBe List(
      List(Symbol("o")),
      List(Symbol("d"), Symbol("e")),
      List(Symbol("d"), Symbol("e")),
      List(Symbol("m"), Symbol("n")),
      List(Symbol("a"), Symbol("b"), Symbol("c")),
      List(Symbol("f"), Symbol("g"), Symbol("h")),
      List(Symbol("i"), Symbol("j"), Symbol("k"), Symbol("l"))
    )

    lsortFreq(
      List(
        List(Symbol("a"), Symbol("b"), Symbol("c")),
        List(Symbol("d"), Symbol("e")),
        List(Symbol("f"), Symbol("g"), Symbol("h")),
        List(Symbol("d"), Symbol("e")),
        List(Symbol("i"), Symbol("j"), Symbol("k"), Symbol("l")),
        List(Symbol("m"), Symbol("n")),
        List(Symbol("o"))
      )
    ) shouldBe List(
      List(Symbol("i"), Symbol("j"), Symbol("k"), Symbol("l")),
      List(Symbol("o")),
      List(Symbol("a"), Symbol("b"), Symbol("c")),
      List(Symbol("f"), Symbol("g"), Symbol("h")),
      List(Symbol("d"), Symbol("e")),
      List(Symbol("d"), Symbol("e")),
      List(Symbol("m"), Symbol("n"))
    )
  }

}
