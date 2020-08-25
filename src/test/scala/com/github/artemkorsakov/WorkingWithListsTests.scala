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

}
