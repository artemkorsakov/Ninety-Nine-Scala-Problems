package com.github.artemkorsakov

object BinaryTrees {
  sealed abstract class Tree[+T] {
    def length: Int
    def isMirrorOf[V](tree: Tree[V]): Boolean
    def isSymmetric: Boolean
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
  }

  case object End extends Tree[Nothing] {
    override def length: Int                                         = 0
    override def isMirrorOf[V](tree: Tree[V]): Boolean               = tree == End
    override def isSymmetric: Boolean                                = true
    override def addValue[U >: Nothing <% Ordered[U]](x: U): Tree[U] = Node(x)
    override def toString                                            = " "
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def length: Int = 1 + left.length + right.length

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

    def symmetricBalancedTrees[T](length: Int, default: T): Set[Tree[T]] =
      cBalanced(length, default).filter(_.isSymmetric)

    def mirror[T](tree: Tree[T]): Tree[T] = tree match {
      case End                      => End
      case Node(value, left, right) => Node(value, mirror(right), mirror(left))
    }

    def fromList[T <% Ordered[T]](list: List[T]): Tree[T] =
      if (list.isEmpty) End
      else {
        fromList(list.dropRight(1)).addValue(list.last)
      }
  }
}
