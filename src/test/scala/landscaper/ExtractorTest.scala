package landscaper

import utest.CompileError.Type
import utest._

object ExtractorTest extends TestSuite {

  val strings = pattern { case x: String => Seq(x) }
  val ints    = pattern { case x: Int    => Seq(x) }
  val all     = pattern { case x: Any    => Seq(x) }

  val tests = Tests {

    "singleton literal extraction" - {
      import extractor.syntax._
      assert("A".extract(strings) == Seq("A"))
      assert(1.extract(strings) == Seq())
      assert(1.extract(ints) == Seq(1))
    }

    "ADT extraction" - {
      sealed trait Node
      case class Named(name: String, node: Literal) extends Node
      case class Literal(content: String)           extends Node

      val lit   = pattern { case x: Literal => Seq(x) }
      val named = pattern { case x: Named   => Seq(x) }

      val data: Seq[Node] =
        Seq(Literal("X"), Named("first", Literal("X")), Literal("Y"))

      assert(extract(strings, data) == Seq("X", "first", "X", "Y"))
      assert(extract(ints, data) == Seq())
      assert(extract(lit, data) == Seq(Literal("X"), Literal("X"), Literal("Y")))

      assert(extract(all, (1, 2.0, "A")) == Seq((1, 2.0, "A"), 1, 2.0, "A"))
    }

    "recursive tree exraction" - {
      sealed trait Tree
      case class Branch(left: Tree, right: Tree) extends Tree
      case class Leaf(content: String)           extends Tree

      val tree = Branch(
        Branch(Leaf("A"), Leaf("B")),
        Branch(Leaf("C"), Leaf("D"))
      )
      assert(extract(strings, tree) == Seq("A", "B", "C", "D"))
    }

    "typing" - {
      // test mostly consist in having those compile
      val x: Seq[String] = extract(strings, Nil)
      val y: Seq[Int]    = extract(ints, (1, "a", 2.0))
      assert(compileError("val x: Seq[Int] = extract(strings, (1, \"a\", 2.0))").isInstanceOf[Type])
    }
  }
}
