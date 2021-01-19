package com.github.artemkorsakov

import com.github.artemkorsakov.MTree._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class MTreeTests extends AnyFlatSpec {
  private val firstTree = MTree(
    'a',
    List(
      MTree('b', List(MTree('e'), MTree('f', List(MTree('g'), MTree('h'), MTree('i'), MTree('j'), MTree('k'))))),
      MTree('c'),
      MTree('d')
    )
  )
  private val secondTree =
    MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))

  "BinaryTrees.toString" should "print correct tree" in {
    firstTree.toString shouldBe "M(a {M(b {M(e {}),M(f {M(g {}),M(h {}),M(i {}),M(j {}),M(k {})})}),M(c {}),M(d {})})"
  }

  "P70C" should "Count the nodes of a multiway tree." in {
    firstTree.nodeCount shouldBe 11
  }

  "P70" should "Tree construction from a node string." in {
    MTree.string2MTree("a^") shouldBe MTree('a')
    MTree.string2MTree("af^^") shouldBe MTree('a', List(MTree('f')))
    MTree.string2MTree("afg^^^") shouldBe MTree('a', List(MTree('f', List(MTree('g')))))

    MTree.string2MTree("afg^^c^bd^e^^^") shouldBe secondTree

    secondTree.toRow shouldBe "afg^^c^bd^e^^^"

    "afg^^c^bd^e^^^".nodeCount shouldBe 7
  }

  "P71" should "Determine the internal path length of a tree." in {
    "afg^^c^bd^e^^^".internalPathLength shouldBe 9
  }

  "P72" should "Construct the postorder sequence of the tree nodes." in {
    "afg^^c^bd^e^^^".postorder shouldBe List('g', 'f', 'c', 'd', 'e', 'b', 'a')
  }

  "P73" should "Lisp-like tree representation." in {
    secondTree.lispyTree shouldBe "(a (f g) c (b d e))"
  }

}
