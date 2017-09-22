package landscaper

import org.scalatest.FreeSpec
import shapeless._
import transformations._

class RewriteTest extends FreeSpec {

  val intToString: Int => String    = (x: Int) => x.toString
  val toLowerCase: String => String = (x: String) => x.toLowerCase

  "literals" - {
    assert(rewrite(intToString, 1) == "1")
    assert(rewrite(intToString, "A") == "A")
    assert(rewrite(toLowerCase, "A") == "a")
    assert(rewrite(toLowerCase, 1) == 1)
  }

  "hlist" - {
    assert(rewrite(intToString, HNil) == HNil)
    assert(rewrite(intToString, 1 :: HNil) == "1" :: HNil)
    assert(rewrite(toLowerCase, 1 :: "A" :: HNil) == 1 :: "a" :: HNil)
    assert(rewrite(intToString, 1 :: "A" :: HNil) == "1" :: "A" :: HNil)
    assert(rewrite(toLowerCase, "1" :: "A" :: HNil) == "1" :: "a" :: HNil)
    assert(rewrite(toLowerCase, rewrite(intToString, "A" :: 1 :: HNil)) == "a" :: "1" :: HNil)
    assert(rewrite(toLowerCase, rewrite(intToString, 1 :: "A" :: HNil)) == "1" :: "a" :: HNil)
  }

  "case-class" - {
    sealed trait BC
    case class A(left: BC, right: BC)
    case class B(s: String)         extends BC
    case class C(i: Int, s: String) extends BC

    "implicit resolution" - {
      Trans[String, String, C]
      Trans[B, C, B :+: CNil]
      Trans[B, C, C :+: CNil]
      Trans[B, C, B :+: C :+: CNil]

      assertDoesNotCompile("Trans[Int, String, C]")

      // FIXME: we should be able to derive rules for this
      // Trans[B, C, BC]
    }

    "typing" - {
      val f    = (b: B) => C(1, b.s)
      val x: C = rewrite(f, B(""))
      val y: C = rewrite(f, C(1, ""))
    }

    "simple rewrites" - {
      assert(rewrite(toLowerCase, C(1, "A")) == C(1, "a"))
      assert(rewrite(toLowerCase, A(B("AA"), C(1, "BB"))) == A(B("aa"), C(1, "bb")))
    }

    "abstract type in nested data structures" - {
      val lifted: BC => BC = {
        case B(s) => C(1, s)
        case x    => x
      }
      assert(rewrite(lifted, B("A"): BC) == C(1, "A"))
      assert(rewrite(lifted, A(B("A"), B("B"))) == A(C(1, "A"), C(1, "B")))
      assert(rewrite(lifted, A(C(1, "A"), B("B"))) == A(C(1, "A"), C(1, "B")))
    }

    "lift from specific to general type" - {
      val g: B => B = {
        case B(s) => B(s.toLowerCase)
      }
      assert(rewrite(g, A(B("A"), B("B"))) == A(B("a"), B("b")))
      assert(rewrite(g, A(C(1, "A"), B("B"))) == A(C(1, "A"), B("b")))
    }
  }

  "collections" - {
    val x: Vector[String]      = rewrite(intToString, Vector(1))
    val y: Vector[Set[String]] = rewrite(intToString, Vector(Set(1)))
    assert(rewrite(toLowerCase, Vector("A")) == Vector("a"))
    assert(rewrite(intToString, Vector(1, 2)) == Vector("1", "2"))

    assertDoesNotCompile("val x: Set[Int] = rewrite(intToString, Set(1))")
  }

  "recursive" - {
    case class N(opt: Option[N])
    Trans[Int, Int, Option[Option[String]]]
    assert(rewrite(toLowerCase, Some(Some("A"))) == Some(Some("a")))

    // FIXME: we should not require explicit type to work
    assert(
      rewrite((x: Option[String]) => (None: Option[String]), Some(Some("A"): Option[String])) == Some(
        None))
    Trans[Int, Int, N]
  }

  "recursive ADT" - {
    sealed trait Tree
    case class Branch(left: Leaves, right: Tree) extends Tree
    sealed trait Leaves                          extends Tree
    case class Leaf(s: String)                   extends Leaves
    case class LeafInt(i: Int)                   extends Leaves
    Generic[Tree]
    Trans[String, String, CNil]
    Trans[String, String, Leaf :+: CNil]
    Trans[String, String, LeafInt :+: Leaf :+: CNil]
    Trans[String, String, Leaves]
    Trans[String, String, Branch]
    Trans[String, String, Tree]
    val tree = Branch(Leaf("A"), Branch(Leaf("B"), Leaf("C")))
    println(Trans[String, String, Tree])
    assert(rewrite(toLowerCase, Leaf("A"): Tree) == Leaf("a"))
    assert(rewrite(toLowerCase, tree) == Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c"))))
  }

  "typing" - {
    // simple typing rules, the following should compile
    val intToString                    = (i: Int) => i.toString
    val res: String                    = rewrite(intToString, 1)
    val res2: String                   = rewrite(intToString, "A")
    val res3: String :: String :: HNil = rewrite(intToString, 1 :: "A" :: HNil)

    assertDoesNotCompile("val res: Int = rewrite(intToString, 1)")

  }

}
