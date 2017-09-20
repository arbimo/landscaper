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

//  test("hlist") {
//    check(
//      rewrite(intToString, HNil),
//      HNil
//    )
//    check(
//      rewrite(intToString, 1 :: HNil),
//      "1" :: HNil
//    )
//    check(
//      rewrite(toLowerCase, 1 :: "A" :: HNil),
//      1 :: "a" :: HNil
//    )
//    check(
//      rewrite(intToString, 1 :: "A" :: HNil),
//      "1" :: "A" :: HNil
//    )
//    check(
//      rewrite(toLowerCase, "1" :: "A" :: HNil),
//      "1" :: "a" :: HNil
//    )
//    val tmp: String :: String :: HNil = rewrite(intToString, 1 :: "A" :: HNil).asInstanceOf[String :: String :: HNil]
//    check(tmp, "1" :: "A" :: HNil)
//    check(
//      rewrite(toLowerCase, tmp),
//      "1" :: "a" :: HNil
//    )
//  }
  test("typing") {
    assertCompiles(
      """
        |//import rewrite._
        |val intToString = (i: Int) => i.toString
        |val res: String = rewrite(intToString, 1)
        |val res2: String = rewrite(intToString, "A")
      """.stripMargin)
    assertDoesNotCompile("val res: Int = rewrite(intToString, 1)")

  }

  Func[Int,String,Int]
//  val res: String = rewrite(intToString, 1)

  test("wip") {
    val s: String = (new ParamFunc[Int,String]).rewrite(intToString, 1)
    val s2: String = Func[Int,String,Int].rewrite(intToString, 1)
  }


}
