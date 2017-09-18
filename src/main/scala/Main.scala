

import shapeless._


sealed trait TNode
case class SNode(id: Int, msg: String) extends TNode
case class INode(id: Int) extends TNode


object Main extends App {

  val n = SNode(1, "coucou")

  val genericSNode = Generic[SNode].to(n)

  println(genericSNode)
  genericSNode


}



object TreeMod extends App {
  import poly._

  // Simple recursive case class family
  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Leaf2[T](ts: Seq[T]) extends Tree[T]
  class Leaf3[T](val s: T) extends Tree[T] {
    override def toString: String = s"Leaf3($s)"
  }
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]


  // Polymorphic function which adds 1 to any Int and is the identity
  // on all other values
  object inc extends -> ((i: Int) => i + 1)
  val incVal = new ->((i: Int) => i+10)


  val tree: Tree[Int] =
    Node(
      new Leaf3(1),
      Node(
        Leaf2(Seq(2, 3, 4)),
        Leaf(3)
      )
    )

  println(everywhere(inc)(tree))
  println(everywhere(incVal)(tree))
}