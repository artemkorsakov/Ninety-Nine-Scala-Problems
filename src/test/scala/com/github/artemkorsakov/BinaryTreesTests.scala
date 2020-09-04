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
    tree.nodeCount shouldBe 7
  }

  "P55" should "Construct completely balanced binary trees." in {
    val list3 = Tree.cBalanced(3, "x")
    list3.foreach(tree => println(tree + "\n__________\n"))
    list3.forall(tree => tree.nodeCount == 3) shouldBe true
    val list4 = Tree.cBalanced(4, "x")
    list4.foreach(tree => println(tree + "\n__________\n"))
    list4.forall(tree => tree.nodeCount == 4) shouldBe true
    val list5 = Tree.cBalanced(5, "x")
    list5.foreach(tree => println(tree + "\n__________\n"))
    list5.forall(tree => tree.nodeCount == 5) shouldBe true
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

  "P60" should "Construct height-balanced binary trees with a given number of nodes." in {
    minHbalNodes(1) shouldBe 1
    maxHbalNodes(1) shouldBe 1

    minHbalNodes(2) shouldBe 2
    maxHbalNodes(2) shouldBe 3

    minHbalNodes(3) shouldBe 4
    maxHbalNodes(3) shouldBe 7

    minHbalNodes(4) shouldBe 7
    maxHbalNodes(4) shouldBe 15

    minHbalHeight(1) shouldBe 1
    maxHbalHeight(1) shouldBe 1

    minHbalHeight(2) shouldBe 2
    maxHbalHeight(2) shouldBe 2

    minHbalHeight(3) shouldBe 2
    maxHbalHeight(3) shouldBe 2

    minHbalHeight(4) shouldBe 3
    maxHbalHeight(4) shouldBe 3

    minHbalHeight(5) shouldBe 3
    maxHbalHeight(5) shouldBe 3

    minHbalHeight(6) shouldBe 3
    maxHbalHeight(6) shouldBe 3

    minHbalHeight(7) shouldBe 3
    maxHbalHeight(7) shouldBe 4
  }

  "P60" should "Construct height-balanced binary trees with a given number of nodes. Part2" in {
    val list4 = Tree.hbalTreesWithNodes(4, "x")
    list4.foreach(tree => println(s"Tree:\n$tree\n"))
    list4.length shouldBe 4

    val list5 = Tree.hbalTreesWithNodes(5, "x")
    list5.foreach(tree => println(s"Tree:\n$tree\n"))
    list5.length shouldBe 6

    val list15 = Tree.hbalTreesWithNodes(15, "x")
    list15.length shouldBe 1553
  }

  "P61" should "Count the leaves of a binary tree." in {
    Node('x', Node('x'), End).leafCount shouldBe 1
  }

  "P61A" should "Collect the leaves of a binary tree in a list." in {
    Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList shouldBe List('b', 'd', 'e')
  }

  "P62" should "Collect the internal nodes of a binary tree in a list." in {
    Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList shouldBe List('a', 'c')
  }

  "P62B" should "Collect the nodes at a given level in a list." in {
    Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(1) shouldBe List('a')
    Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2) shouldBe List('b', 'c')
    Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(3) shouldBe List('d', 'e')
  }

  "P63" should "Construct a complete binary tree." in {
    val tree = Tree.completeBinaryTree(6, "x")
    println(tree)
  }

  "P64" should "Layout a binary tree (1)." in {
    val posnode = Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree
    posnode.toString shouldBe "T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))"

    val tree = Tree.fromList(List('n', 'k', 'm', 'c', 'a', 'h', 'g', 'e', 'u', 'p', 's', 'q'))
    tree.layoutBinaryTree.toString shouldBe "T[8,1](n T[6,2](k T[2,3](c T[1,4](a . .) T[5,4](h T[4,5](g T[3,6](e . .) .) .)) T[7,3](m . .)) T[12,2](u T[9,3](p . T[11,4](s T[10,5](q . .) .)) .))"
  }

  "P65" should "Layout a binary tree (2)." in {
    Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2.toString shouldBe "T[3,1](a T[1,2](b . T[2,3](c . .)) T[5,2](d . .))"
  }

  "P66" should "Layout a binary tree (3)." in {
    Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree3.toString shouldBe "T[2,1](a T[1,2](b . T[2,3](c . .)) T[3,2](d . .))"
  }

  "P67" should "A string representation of binary trees." in {
    Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toStringPres shouldBe "a(b(d,e),c(,f(g,)))"

    Tree.fromString("a(b(d,e),c(,f(g,)))") shouldBe Node(
      'a',
      Node('b', Node('d'), Node('e')),
      Node('c', End, Node('f', Node('g'), End))
    )
  }

  "P68" should "Preorder and inorder sequences of binary trees." in {
    Tree.fromString("a(b(d,e),c(,f(g,)))").preorder shouldBe List('a', 'b', 'd', 'e', 'c', 'f', 'g')

    Tree.fromString("a(b(d,e),c(,f(g,)))").inorder shouldBe List('d', 'b', 'e', 'a', 'c', 'g', 'f')

    val tree = Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
    tree shouldBe Node('a', Node('b', Node('d'), Node('e')), Node('c', Node('f'), Node('g')))
  }

  "P69" should "Dotstring representation of binary trees." in {
    Tree.fromString("a(b(d,e),c(,f(g,)))").toDotstring shouldBe "abd..e..c.fg..."

    Tree.fromDotstring("abd..e..c.fg...") shouldBe Node(
      'a',
      Node('b', Node('d'), Node('e')),
      Node('c', End, Node('f', Node('g'), End))
    )
  }

}
