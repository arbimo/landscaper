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
    check(
      rewrite(toLowerCase, rewrite(intToString, 1 :: "A" :: HNil)),
      "1" :: "a" :: HNil
    )
  }


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
