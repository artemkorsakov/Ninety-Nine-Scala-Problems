package com.github.artemkorsakov

/**
  * A binary tree is either empty or it is composed of a root element and two successors, which are binary trees themselves.
  *
  * We shall use the following classes to represent binary trees. (Also available in tree1.scala.)
  * An End is equivalent to an empty tree. A Branch has a value, and two descendant trees.
  * The toString functions are relatively arbitrary, but they yield a more compact output than Scala's default.
  * Putting a plus in front of the T makes the class covariant; it will be able to hold subtypes of whatever type
  * it's created for. (This is important so that End can be a singleton object; as a singleton,
  * it must have a specific type, so we give it type Nothing, which is a subtype of every other type.)
  *
  * sealed abstract class Tree[+T]
  * case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  * override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  * }
  * case object End extends Tree[Nothing] {
  * override def toString = "."
  * }
  * object Node {
  * def apply[T](value: T): Node[T] = Node(value, End, End)
  * }
  * The example tree on the right is given by
  *
  * Node('a',
  * Node('b', Node('d'), Node('e')),
  * Node('c', End, Node('f', Node('g'), End)))
  * A tree with only a root node would be Node('a') and an empty tree would be End.
  *
  * Throughout this section, we will be adding methods to the classes above, mostly to Tree.
  */
object BinaryTrees {
  sealed abstract class Tree[+T] {
    def nodeCount: Int
    def height: Int

    /** A leaf is a node with no successors. Write a method leafCount to count them.
      */
    def leafCount: Int

    /** A leaf is a node with no successors. Write a method leafList to collect them in a list.
      */
    def leafList: List[T]

    /** An internal node of a binary tree has either one or two non-empty successors. Write a method internalList to collect them in a list.
      */
    def internalList: List[T]

    /** Collect the nodes at a given level in a list.
      * A node of a binary tree is at level N if the path from the root to the node has length N-1.
      * The root node is at level 1. Write a method atLevel to collect all nodes at a given level in a list.
      */
    def atLevel(level: Int): List[T]
    def isMirrorOf[V](tree: Tree[V]): Boolean

    /**
      * Symmetric binary trees.
      * Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Add an isSymmetric method to the Tree class to check whether a given binary tree is symmetric. Hint: Write an isMirrorOf method first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
      * scala> Node('a', Node('b'), Node('c')).isSymmetric
      * res0: Boolean = true
      */
    def isSymmetric: Boolean

    /**
      * Binary search trees (dictionaries).
      * Write a function to add an element to a binary search tree.
      * Hint: The abstract definition of addValue in Tree should be def addValue[U >: T <% Ordered[U]](x: U): Tree[U]. The >: T is because addValue's parameters need to be contravariant in T. (Conceptually, we're adding nodes above existing nodes. In order for the subnodes to be of type T or any subtype, the upper nodes must be of type T or any supertype.) The <% Ordered[U] allows us to use the < operator on the values in the tree.
      */
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U]

    /** As a preparation for drawing a tree, a layout algorithm is required to determine the position of each node in a rectangular grid.
      * Several layout methods are conceivable, one of them is shown in the illustration on the right.
      * In this layout strategy, the position of a node v is obtained by the following two rules:
      * x(v) is equal to the position of the node v in the inorder sequence
      * y(v) is equal to the depth of the node v in the tree
      * In order to store the position of the nodes, we add a new class with the additional information.
      */
    def layoutBinaryTree: Tree[T] = layoutBinaryTreeInternal(1, 1)._1
    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int)

    def treeDepth: Int
    def leftmostNodeDepth: Int

    /** An alternative layout method is depicted in the illustration opposite.
      * Find out the rules and write the corresponding method. Hint: On a given level, the horizontal distance between neighboring nodes is constant.
      */
    def layoutBinaryTree2: Tree[T] = {
      val d  = treeDepth
      val x0 = (2 to leftmostNodeDepth).map(n => Math.pow(2, d - n).toInt).sum + 1
      layoutBinaryTree2Internal(x0, 1, d - 2)
    }
    def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T]
  }

  case object End extends Tree[Nothing] {
    override def nodeCount: Int                                                = 0
    override def height: Int                                                   = 0
    override def leafCount: Int                                                = 0
    override def leafList: List[Nothing]                                       = Nil
    override def internalList: List[Nothing]                                   = Nil
    override def atLevel(level: Int): List[Nothing]                            = Nil
    override def isMirrorOf[V](tree: Tree[V]): Boolean                         = tree == End
    override def isSymmetric: Boolean                                          = true
    override def addValue[U >: Nothing <% Ordered[U]](x: U): Tree[U]           = Node(x)
    override def toString                                                      = "."
    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[Nothing], Int)     = (End, x)
    def treeDepth: Int                                                         = 0
    def leftmostNodeDepth: Int                                                 = 0
    def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[Nothing] = End
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def nodeCount: Int = 1 + left.nodeCount + right.nodeCount
    override def height: Int    = 1 + math.max(left.height, right.height)
    override def leafCount: Int = (left, right) match {
      case (End, End) => 1
      case _          => left.leafCount + right.leafCount
    }
    override def leafList: List[T] = (left, right) match {
      case (End, End) => List(value)
      case _          => left.leafList ::: right.leafList
    }
    override def internalList: List[T] = (left, right) match {
      case (End, End) => Nil
      case _          => value :: left.internalList ::: right.internalList
    }
    override def atLevel(level: Int): List[T] = level match {
      case n if n < 1 => Nil
      case 1          => List(value)
      case l          => left.atLevel(l - 1) ::: right.atLevel(l - 1)
    }

    def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
      case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
      case _          => false
    }

    def isSymmetric: Boolean = left.isMirrorOf(right)

    override def addValue[U >: T <% Ordered[U]](x: U): Tree[U] =
      if (x < value) {
        Node(value, left.addValue(x), right)
      } else {
        Node(value, left, right.addValue(x))
      }

    override def toString: String =
      if (left.equals(End) && right.equals(End)) {
        value.toString
      } else {
        val str       = value.toString
        val spaces    = List.fill(str.length)(" ").mkString
        val valueL    = if (left.equals(End)) Array.empty[String] else left.toString.split("\n")
        val spacesL   = if (left.equals(End)) "" else List.fill(valueL.head.length)(" ").mkString
        val valueR    = if (right.equals(End)) Array.empty[String] else right.toString.split("\n")
        val spacesR   = if (right.equals(End)) "" else List.fill(valueR.head.length)(" ").mkString
        val firstRow  = s"$spacesL $str $spacesR"
        val secondRow = s"$spacesL/$spaces\\$spacesR"
        val l         = math.max(valueL.length, valueR.length)
        val lastRows = (0 until l).map { i =>
          val vall = if (i < valueL.length) valueL(i) else spacesL
          val valr = if (i < valueR.length) valueR(i) else spacesR
          s"$vall $spaces $valr"
        }
        (Array(firstRow) ++ Array(secondRow) ++ lastRows).mkString("\n")
      }

    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
      val (leftTree, myX)    = left.layoutBinaryTreeInternal(x, depth + 1)
      val (rightTree, nextX) = right.layoutBinaryTreeInternal(myX + 1, depth + 1)
      (PositionedNode(value, leftTree, rightTree, myX, depth), nextX)
    }
    def treeDepth: Int         = (left.treeDepth max right.treeDepth) + 1
    def leftmostNodeDepth: Int = left.leftmostNodeDepth + 1
    def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T] =
      PositionedNode(
        value,
        left.layoutBinaryTree2Internal(x - math.pow(2, exp).toInt, depth + 1, exp - 1),
        right.layoutBinaryTree2Internal(x + math.pow(2, exp).toInt, depth + 1, exp - 1),
        x,
        depth
      )
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  case class PositionedNode[+T](
      value: T,
      left: Tree[T],
      right: Tree[T],
      x: Int,
      y: Int
  ) extends Tree[T] {
    private val node                                                          = Node(value, left, right)
    override def nodeCount: Int                                               = node.nodeCount
    override def height: Int                                                  = node.height
    override def leafCount: Int                                               = node.leafCount
    override def leafList: List[T]                                            = node.leafList
    override def internalList: List[T]                                        = node.internalList
    override def atLevel(level: Int): List[T]                                 = node.atLevel(level)
    override def isMirrorOf[V](tree: Tree[V]): Boolean                        = node.isMirrorOf(tree)
    override def isSymmetric: Boolean                                         = node.isSymmetric
    override def addValue[U >: T <% Ordered[U]](x: U): Tree[U]                = node.addValue(x)
    override def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = node.layoutBinaryTreeInternal(x, depth)
    def treeDepth: Int                                                        = node.treeDepth
    def leftmostNodeDepth: Int                                                = node.leftmostNodeDepth
    def layoutBinaryTree2Internal(x: Int, depth: Int, exp: Int): Tree[T]      = node.layoutBinaryTree2Internal(x, depth, exp)
    override def toString: String =
      "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  object Tree {

    /**
      * Construct completely balanced binary trees.
      * In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.
      * Define an object named Tree. Write a function Tree.cBalanced to construct completely balanced binary trees for a given number of nodes. The function should generate all solutions. The function should take as parameters the number of nodes and a single value to put in all of them.
      *
      * scala> Tree.cBalanced(4, "x")
      * res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...
      */
    def cBalanced[T](length: Int, default: T): Set[Tree[T]] =
      if (length == 0) {
        Set(End)
      } else if (length == 1) {
        Set(Node(default))
      } else {
        val n     = length / 2
        val treeB = cBalanced[T](n, default)
        val treeS = if (length % 2 == 0) cBalanced[T](n - 1, default) else treeB
        (for {
          a <- treeS
          b <- treeB
        } yield Node(default, a, b) :: Node(default, b, a) :: Nil).flatten
      }

    /**
      * Generate-and-test paradigm.
      * Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
      * scala> Tree.symmetricBalancedTrees(5, "x")
      * res0: List[Node[String]] = List(T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .))))
      */
    def symmetricBalancedTrees[T](length: Int, default: T): Set[Tree[T]] =
      cBalanced(length, default).filter(_.isSymmetric)

    /** Construct height-balanced binary trees.
      * In a height-balanced binary tree, the following property holds for every node:
      * The height of its left subtree and the height of its right subtree are almost equal,
      * which means their difference is not greater than one.
      */
    def hbalTrees[T](height: Int, value: T): List[Tree[T]] = height match {
      case n if n < 1 => List(End)
      case 1          => List(Node(value))
      case _ =>
        val fullHeight = hbalTrees(height - 1, value)
        val short      = hbalTrees(height - 2, value)
        fullHeight.flatMap(l => fullHeight.map(r => Node(value, l, r))) :::
        fullHeight.flatMap(f => short.flatMap(s => List(Node(value, f, s), Node(value, s, f))))
    }

    /**
      * Construct height-balanced binary trees with a given number of nodes.
      * Consider a height-balanced binary tree of height H.
      * What is the maximum number of nodes it can contain? Clearly, MaxN = 2H - 1.
      * However, what is the minimum number MinN?
      */
    def minHbalNodes(height: Int): Int = height match {
      case n if n < 1 => 0
      case 1          => 1
      case _          => 1 + minHbalNodes(height - 1) + minHbalNodes(height - 2)
    }
    def maxHbalNodes(height: Int): Int = math.pow(2, height).toInt - 1

    /**
      * On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have?
      */
    def minHbalHeight(nodes: Int): Int =
      if (nodes == 0) 0
      else minHbalHeight(nodes / 2) + 1
    def maxHbalHeight(nodes: Int): Int =
      LazyList.from(1).takeWhile(minHbalNodes(_) <= nodes).last

    def hbalTreesWithNodes[T](nodes: Int, value: T): List[Tree[T]] =
      (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, value)).filter(_.nodeCount == nodes).toList

    def mirror[T](tree: Tree[T]): Tree[T] = tree match {
      case End                      => End
      case Node(value, left, right) => Node(value, mirror(right), mirror(left))
    }

    def fromList[T <% Ordered[T]](list: List[T]): Tree[T] =
      list.foldLeft(End: Tree[T])((r, e) => r.addValue(e))

    /** Construct a complete binary tree.
      * A complete binary tree with height H is defined as follows: The levels 1,2,3,...,H-1 contain the maximum number
      * of nodes (i.e 2(i-1) at the level i, note that we start counting the levels from 1 at the root).
      * In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted".
      * This means that in a levelorder tree traversal all internal nodes come first, the leaves come second,
      * and empty successors (the Ends which are not really nodes!) come last.
      * Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.
      * We can assign an address number to each node in a complete binary tree by enumerating the nodes in levelorder,
      * starting at the root with number 1. In doing so, we realize that for every node X with address A
      * the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively,
      * supposed the successors do exist. This fact can be used to elegantly construct a complete binary tree structure.
      * Write a method completeBinaryTree that takes as parameters the number of nodes and the value to put in each node.
      */
    def completeBinaryTree[T](nodes: Int, value: T): Tree[T] = {
      def generateTree(addr: Int): Tree[T] =
        if (addr > nodes) End
        else Node(value, generateTree(2 * addr), generateTree(2 * addr + 1))
      generateTree(1)
    }
  }
}
