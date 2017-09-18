

object Extractor extends App {
  import shapeless._

  sealed trait Node
  case class Named(name: String, node: Literal) extends Node
  case class Literal(content: String) extends Node

  trait PatternFinder[In] {
    type Pattern
    def patternMatch(in: In): Seq[Pattern]
  }
  object PatternFinder {
    type Aux[In,Out] = PatternFinder[In] { type Pattern }

    def apply[In,Out](f: In => Seq[Out]): Aux[In,Out] = new PatternFinder[In] {
      type Pattern = Out
      override def patternMatch(in: In): Seq[Pattern] = f(in)
    }
  }


  trait Extractor[In] {
    def extract(f: PatternFinder[Any])(t: In): Seq[f.Pattern]
  }
  object Extractor {
    def apply[T](implicit ext: Extractor[T]): Extractor[T] = ext
    def terminal[T]: Extractor[T] = new Extractor[T] {
      override def extract(f: PatternFinder[Any])(t: T): Seq[f.Pattern] =
        f.patternMatch(t)
    }

    implicit def extractString = terminal[String]
    implicit def extractInt = terminal[Int]
    implicit def extractHNil = terminal[HNil]
    implicit def extractCNil = terminal[CNil]

    implicit def extractHList[H, T <: HList](implicit hExt: Extractor[H], tExt: Extractor[T]) : Extractor[H :: T] =
      new Extractor[H::T] {
        override def extract(f: PatternFinder[Any])(l: H::T): Seq[f.Pattern] =
          f.patternMatch(l.head) ++ hExt.extract(f)(l.head) ++ tExt.extract(f)(l.tail)
      }
    implicit def extractGen[T, R](implicit gen: Generic.Aux[T,R], ext: Extractor[R]): Extractor[T] =
      new Extractor[T] {
        override def extract(f: PatternFinder[Any])(t: T): Seq[f.Pattern] =
          f.patternMatch(t) ++ ext.extract(f)(gen.to(t))
      }
  }

  val pat: PatternFinder[Any] = PatternFinder((x: Any) => x match {
    case s: String => Seq(s)
    case _ => Seq()
  })
  val all = PatternFinder(Seq(_: Any))
  println(Extractor[String :: String :: HNil].extract(all)("Coucou" :: "Caca" :: HNil))
  println(Extractor[String].extract(pat)("coucou"))
  println(Extractor[(Int,String)].extract(all)((1, "A")))
}

