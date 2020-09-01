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
    def length: Int
    def height: Int
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
  }

  case object End extends Tree[Nothing] {
    override def length: Int                                         = 0
    override def height: Int                                         = 0
    override def isMirrorOf[V](tree: Tree[V]): Boolean               = tree == End
    override def isSymmetric: Boolean                                = true
    override def addValue[U >: Nothing <% Ordered[U]](x: U): Tree[U] = Node(x)
    override def toString                                            = "."
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def length: Int = 1 + left.length + right.length
    override def height: Int = 1 + math.max(left.height, right.height)

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
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
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
      * Generate-and-test paradigm.
      * Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
      * scala> Tree.symmetricBalancedTrees(5, "x")
      * res0: List[Node[String]] = List(T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .))))
      */
    def symmetricBalancedTrees[T](length: Int, default: T): Set[Tree[T]] =
      cBalanced(length, default).filter(_.isSymmetric)

    def mirror[T](tree: Tree[T]): Tree[T] = tree match {
      case End                      => End
      case Node(value, left, right) => Node(value, mirror(right), mirror(left))
    }

    def fromList[T <% Ordered[T]](list: List[T]): Tree[T] =
      list.foldLeft(End: Tree[T])((r, e) => r.addValue(e))
  }
}
