import org.scalatest.FunSuite

class RewriteTest extends FunSuite {
  import shapeless._
  import rewrite._

  val intToString = (x: Int) => x.toString
  val toLowerCase: String => String = (x: String) => x.toLowerCase

  def check[T](in: T, out: T): Unit = {
    assert(in === out)
  }

  test("literals") {
    check(
      rewrite(intToString, 1),
      "1"
    )
    check(
      rewrite(intToString, "A"),
      "A"
    )
    check(
      rewrite(toLowerCase, "A"),
      "a"
    )
    check(
      rewrite(toLowerCase, 1),
      1
    )
  }

  test("hlist") {
    check(
      rewrite(intToString, HNil),
      HNil
    )
    check(
      rewrite(intToString, 1 :: HNil),
      "1" :: HNil
    )
    check(
      rewrite(toLowerCase, 1 :: "A" :: HNil),
      1 :: "a" :: HNil
    )
    check(
      rewrite(intToString, 1 :: "A" :: HNil),
      "1" :: "A" :: HNil
    )
    check(
      rewrite(toLowerCase, "1" :: "A" :: HNil),
      "1" :: "a" :: HNil
    )
    check(
      rewrite(toLowerCase, rewrite(intToString, "A" :: 1 :: HNil)),
      "a" :: "1" :: HNil
    )
    assertResult("1" :: "a" :: HNil)(
      rewrite(toLowerCase, rewrite(intToString, 1 :: "A" :: HNil))
    )
  }

  test("case-class") {
    sealed trait AB
    case class A(left: AB, right: AB)
    case class B(s: String) extends AB
    case class C(i: Int, s: String) extends AB
    assertCompiles("Trans[String,String,C]")
    assertDoesNotCompile("Trans[Int, String, C]")

    check(
      rewrite(toLowerCase, C(1, "A")),
      C(1, "a")
    )
    check(
      rewrite(toLowerCase, A(B("AA"), C(1, "BB"))),
      A(B("aa"), C(1, "bb"))
    )
    val f = (b: B) => C(1, b.s)
    check(rewrite(f, B("A"): AB), C(1, "A"))
    check(rewrite(f, A(B("A"), B("B"))), A(C(1, "A"), C(1, "B")))
    check(rewrite(f, A(C(1, "A"), B("B"))), A(C(1, "A"), C(1, "B")))
  }

  test("collections") {
    check(rewrite(toLowerCase, Vector("A")), Vector("a"))
    check(rewrite(intToString, Vector(1, 2)), Vector("1", "2"))
    assertCompiles("val x: Vector[String] = rewrite(intToString, Vector(1))")
    assertDoesNotCompile("val x: Set[Int] = rewrite(intToString, Set(1))")
  }

//  test("recursive ADT") {
//    sealed trait Tree
//    case class Branch(left: Leaves, right: Tree) extends Tree
//    sealed trait Leaves extends Tree
//    case class Leaf(s: String) extends Leaves
//    case class LeafInt(i: Int) extends Leaves
//    assertCompiles("Generic[Tree]")
//    assertCompiles("Trans[String,String, CNil]")
//    assertCompiles("Trans[String,String, Leaf :+: CNil]")
//    assertCompiles("Trans[String,String, LeafInt :+: Leaf :+: CNil]")
//    assertCompiles("Trans[String,String, Leaves]")
//    assertCompiles("Trans[String,String, Branch]")
//    assertCompiles("Trans[String,String, Tree]")
//    val tree = Branch(Leaf("A"), Branch(Leaf("B"), Leaf("C")))
//    println(Trans[String,String,Tree])
//    check(
//      rewrite(toLowerCase, Leaf("A"): Tree),
//      Leaf("a")
//    )
//    check(
//      rewrite(toLowerCase, tree),
//      Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c")))
//    )
//  }

  test("typing") {
    assertCompiles(
      """
        |val intToString = (i: Int) => i.toString
        |val res: String = rewrite(intToString, 1)
        |val res2: String = rewrite(intToString, "A")
        |val res3: String :: String :: HNil = rewrite(intToString, 1 :: "A" :: HNil)
      """.stripMargin)
    assertDoesNotCompile("val res: Int = rewrite(intToString, 1)")

  }

}
