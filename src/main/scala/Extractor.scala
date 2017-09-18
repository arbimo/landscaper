

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
    final def extract(f: PatternFinder[Any])(t: In): Seq[f.Pattern] = extractSelf(f)(t) ++ extractInner(f)(t)
    protected def extractInner(f: PatternFinder[Any])(t: In): Seq[f.Pattern]
    final protected def extractSelf(f: PatternFinder[Any])(t: In): Seq[f.Pattern] = f.patternMatch(t)
  }
  object Extractor {
    def apply[T](implicit ext: Extractor[T]): Extractor[T] = ext
    def terminal[T]: Extractor[T] = new Extractor[T] {
      override protected def extractInner(f: PatternFinder[Any])(t: T): Seq[f.Pattern] = Seq()
    }

    implicit def extractString = terminal[String]
    implicit def extractInt = terminal[Int]
    implicit def extractHNil = terminal[HNil]
    implicit def extractCNil = terminal[CNil]

    implicit def extractHList[H, T <: HList](implicit hExt: Extractor[H], tExt: Extractor[T]) : Extractor[H :: T] =
      new Extractor[H::T] {
        override def extractInner(f: PatternFinder[Any])(l: H::T): Seq[f.Pattern] =
          hExt.extract(f)(l.head) ++ tExt.extractInner(f)(l.tail)
      }
    implicit def extractGen[T, R](implicit gen: Generic.Aux[T,R], ext: Extractor[R]): Extractor[T] =
      new Extractor[T] {
        override def extractInner(f: PatternFinder[Any])(t: T): Seq[f.Pattern] =
          ext.extractInner(f)(gen.to(t))
      }
  }

  val pat: PatternFinder[Any] = PatternFinder((x: Any) => x match {
    case s: String => Seq(s)
    case _ => Seq()
  })
  val all: PatternFinder[Any] = PatternFinder(Seq(_: Any))
  println(Extractor[String :: String :: HNil].extract(all)("Coucou" :: "Caca" :: HNil))
  println(Extractor[String].extract(pat)("coucou"))
  println(Extractor[(Int,String)].extract(all)((1, "A")))
  println(Extractor[((Int,String),String)].extract(all)(((1,"AA"), "A")))
}

