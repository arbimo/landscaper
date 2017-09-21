package landscaper

import landscaper.eraser._
import org.scalatest.FunSuite

class EraserTest extends FunSuite {
  type x = Int =:= String

  val aPred: Predicate = predicate { case x: String => x.startsWith("A") }

  test("literal") {
    assert(erase(predicate { case _: String => true })("coucou") == "coucou")
  }

  test("seq") {
    assert(
      erase(predicate { case x: String => x == "A" })(Seq("A", "B")) == Seq(
        "B"))
  }

  test("seq-seq") {
    assert(erase(aPred)(Seq(Seq("A", "B", "AA"))) == Seq(Seq("B")))
  }

  test("tuple-seq") {
    assert(erase(aPred)((Seq("A", "B"), Seq(1, 2))) == (Seq("B"), Seq(1, 2)))
  }

  test("recursive tree erase") {
    sealed trait Tree
    case class Branch(left: Tree, right: Tree) extends Tree
    case class Leaf(content: Seq[String]) extends Tree

    val tree = Branch(
      Branch(Leaf(Seq("A")), Leaf(Seq("B"))),
      Leaf(Seq("A", "B"))
    )
    assert(
      erase(aPred)(tree) == Branch(Branch(Leaf(Seq()), Leaf(Seq("B"))),
                                   Leaf(Seq("B"))))
  }
}
