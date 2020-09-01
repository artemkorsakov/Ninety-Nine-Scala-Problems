package com.github.artemkorsakov

import com.github.artemkorsakov.BinaryTrees.Tree._
import com.github.artemkorsakov.BinaryTrees._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BinaryTreesTests extends AnyFlatSpec {
  "BinaryTrees.toString" should "print correct tree" in {
    println("Tree:\n" + Node('f', End, End).toString)
    println("Tree:\n" + Node('f', Node('g'), End).toString)
    println("Tree:\n" + Node('f', End, Node('g')).toString)
    println("Tree:\n" + Node('b', Node('d'), Node('e')).toString)
    val tree = Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))
    println("Tree:\n" + tree.toString)
    tree.length shouldBe 7
  }

  "P55" should "Construct completely balanced binary trees." in {
    val list3 = Tree.cBalanced(3, "x")
    list3.foreach(tree => println(tree + "\n__________\n"))
    list3.forall(tree => tree.length == 3) shouldBe true
    val list4 = Tree.cBalanced(4, "x")
    list4.foreach(tree => println(tree + "\n__________\n"))
    list4.forall(tree => tree.length == 4) shouldBe true
    val list5 = Tree.cBalanced(5, "x")
    list5.foreach(tree => println(tree + "\n__________\n"))
    list5.forall(tree => tree.length == 5) shouldBe true
  }

  "P56" should "Symmetric binary trees." in {
    mirror(Node('a', Node('b'), Node('c'))) shouldBe Node('a', Node('c'), Node('b'))

    Node('a', Node('b'), Node('c')).isSymmetric shouldBe true
  }

  "P57" should "Binary search trees (dictionaries)." in {
    val res0 = End.addValue(2)
    res0 shouldBe Node(2, End, End)

    val res1 = res0.addValue(3)
    res1 shouldBe Node(2, End, Node(3, End, End))

    res1.addValue(0) shouldBe Node(2, Node(0, End, End), Node(3, End, End))

    Tree.fromList(List(3, 2, 5, 7, 1)) shouldBe Node(
      3,
      Node(2, Node(1, End, End), End),
      Node(5, End, Node(7, End, End))
    )

    Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric shouldBe true

    Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric shouldBe false
  }

  "P58" should "Generate-and-test paradigm." in {
    val list = Tree.symmetricBalancedTrees(5, "x")
    list.foreach(tree => println(tree))
  }

  "P59" should "Construct height-balanced binary trees." in {
    (0 to 4).foreach { i =>
      println(s"Trees $i:")
      val list = Tree.hbalTrees(i, "x")
      list.size shouldBe Array(1, 1, 3, 15, 315)(i)
      list.foreach(tree => println(s"Tree:\n$tree\n"))
    }
  }

  "P60" should "" in {}

  "P61" should "" in {}

  "P61A" should "" in {}

  "P62" should "" in {}

  "P62B" should "" in {}

  "P63" should "" in {}

  "P64" should "" in {}

  "P65" should "" in {}

  "P66" should "" in {}

  "P67" should "" in {}

  "P68" should "" in {}

  "P69" should "" in {}

}
