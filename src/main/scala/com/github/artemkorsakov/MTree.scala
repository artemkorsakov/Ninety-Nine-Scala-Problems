package com.github.artemkorsakov

/** A multiway tree is composed of a root element and a (possibly empty) set of successors
  * which are multiway trees themselves.
  * A multiway tree is never empty. The set of successor trees is sometimes called a forest.
  */
case class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List.empty[MTree[T]])

  /** Count the nodes of a multiway tree. */
  def nodeCount: Int = children.foldLeft(1)(_ + _.nodeCount)

  /** Determine the internal path length of a tree. */
  def internalPathLength: Int =
    children.foldLeft(0)((sum, tree) => sum + tree.internalPathLength + tree.nodeCount)

  /** Construct the postorder sequence of the tree nodes. */
  def postorder: List[T] =
    children.flatMap(_.postorder) :+ value

  /** Lisp-like tree representation. */
  def lispyTree: String =
    if (children.isEmpty)
      value.toString
    else
      s"($value ${children.map(_.lispyTree).mkString(" ")})"

  def toRow: String = s"$value${children.map(_.toRow).mkString("")}^"

  override def toString: String = s"M($value {${children.map(_.toString).mkString(",")}})"
}

object MTree {
  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)
  def apply[T](value: T)                           = new MTree(value, List.empty[MTree[T]])

  implicit def string2MTree(str: String): MTree[Char] = {
    def splitMid(mid: String): List[String] =
      if (mid.isEmpty) {
        List.empty[String]
      } else {
        var count = 1
        var idx   = 1
        while (count > 0) {
          count += (if (mid(idx) == '^') -1 else 1)
          idx += 1
        }
        mid.take(idx) +: splitMid(mid.drop(idx))
      }
    val mid = str.tail.init
    MTree(str.head, splitMid(mid).map(string2MTree))
  }

}
