package landscaper

import landscaper.eraser._
import utest.CompileError.Type
import utest._

object EraserTest extends TestSuite {

  val aPred: Predicate = predicate { case x: String => x.startsWith("A") }

  val tests = Tests {

    "literal" - {
      assert(erase(predicate { case _: String => true })("coucou") == "coucou")
    }

    "seq" - {
      assert(erase(predicate { case x: String => x == "A" })(Seq("A", "B")) == Seq("B"))
    }

    "seq-seq" - {
      assert(erase(aPred)(Seq(Seq("A", "B", "AA"))) == Seq(Seq("B")))
    }

    "seq-set" - {
      assert(erase(aPred)(Seq(Set("A", "B", "AA"))) == Seq(Set("B")))
      val x: Seq[Set[String]] = erase(aPred)(Seq(Set("A")))
      assert(
        compileError("val x: Seq[Seq[String]] = erase(aPred)(Seq(Set(\"A\")))").isInstanceOf[Type])
    }

    "tuple-seq" - {
      assert(erase(aPred)((Seq("A", "B"), Seq(1, 2))) == (Seq("B"), Seq(1, 2)))
    }

    "recursive tree erase" - {
      sealed trait Tree
      case class Branch(left: Tree, right: Tree) extends Tree
      case class Leaf(content: Seq[String])      extends Tree

      val tree = Branch(
        Branch(Leaf(Seq("A")), Leaf(Seq("B"))),
        Leaf(Seq("A", "B"))
      )
      assert(erase(aPred)(tree) == Branch(Branch(Leaf(Seq()), Leaf(Seq("B"))), Leaf(Seq("B"))))
    }
  }
}
