import org.scalatest.FunSuite
import witness.IterableWitness

package object extraction {

  import shapeless._

  trait PatternFinder[In] {
    type Pattern

    def patternMatch(in: In): Seq[Pattern]
  }

  object PatternFinder {
    type Aux[In, Out] = PatternFinder[In] { type Pattern }

    def apply[In, Out](f: In => Seq[Out]): Aux[In, Out] =
      new PatternFinder[In] {
        type Pattern = Out

        override def patternMatch(in: In): Seq[Pattern] = f(in)
      }

    def apply[In, Out](f: PartialFunction[In, Seq[Out]]): Aux[In, Out] =
      new PatternFinder[In] {
        type Pattern = Out

        override def patternMatch(in: In): Seq[Pattern] =
          if (f.isDefinedAt(in)) f(in) else Seq()
      }
  }

  def pattern[Out](
      pf: PartialFunction[Any, Seq[Out]]): PatternFinder.Aux[Any, Out] =
    PatternFinder(pf)

  trait Extractor[In] {

    /** Lookup for pattern in t and all constituent of t */
    final def extract(f: PatternFinder[Any])(t: In): Seq[f.Pattern] =
      f.patternMatch(t) ++ extractInner(f)(t)

    /** Extract from all constituent of t, ignoring t itself */
    protected def extractInner(f: PatternFinder[Any])(t: In): Seq[f.Pattern]
  }

  object Extractor {
    def apply[T](implicit ext: Extractor[T]): Extractor[T] = ext

    def terminal[T]: Extractor[T] = new Extractor[T] {
      override protected def extractInner(f: PatternFinder[Any])(
          t: T): Seq[f.Pattern] = Seq()
    }

    // anything identified as a literal is terminal node
    implicit def extractLiteral[T: witness.LiteralWitness]: Extractor[T] =
      terminal[T]

    implicit def extractHList[H, T <: HList](
        implicit hExt: Lazy[Extractor[H]],
        tExt: Extractor[T]): Extractor[H :: T] =
      new Extractor[H :: T] {
        override def extractInner(f: PatternFinder[Any])(
            l: H :: T): Seq[f.Pattern] =
          hExt.value.extract(f)(l.head) ++ tExt.extractInner(f)(l.tail)
      }

    implicit def extractCoproduct[H, T <: Coproduct](
        implicit hExt: Lazy[Extractor[H]],
        tExt: Extractor[T]): Extractor[H :+: T] =
      new Extractor[H :+: T] {
        override protected def extractInner(f: PatternFinder[Any])(
            t: H :+: T): Seq[f.Pattern] =
          t match {
            case Inl(x) => hExt.value.extractInner(f)(x)
            case Inr(x) => tExt.extractInner(f)(x)
          }
      }

    /** Extractor for anything wit a Generic representation */
    implicit def extractGen[T, R](implicit gen: Generic.Aux[T, R],
                                  ext: Lazy[Extractor[R]]): Extractor[T] =
      new Extractor[T] {
        override def extractInner(f: PatternFinder[Any])(t: T): Seq[f.Pattern] =
          ext.value.extractInner(f)(gen.to(t))
      }

    implicit def extractIterable[T](
        implicit ext: Extractor[T]): Extractor[Iterable[T]] =
      new Extractor[Iterable[T]] {
        override protected def extractInner(f: PatternFinder[Any])(
            t: Iterable[T]): Seq[f.Pattern] =
          t.flatMap(e => ext.extract(f)(e)).toSeq
      }

    /** Extract any collection explicitly flagged by an IterableWitness.
      * This is mainly to avoid overloading implici for types such as HList. */
    implicit def extractCollection[Content, Coll](
        implicit iter: IterableWitness[Coll, Content],
        ext: Extractor[Iterable[Content]]): Extractor[Coll] =
      new Extractor[Coll] {
        override protected def extractInner(f: PatternFinder[Any])(
            t: Coll): Seq[f.Pattern] =
          ext.extractInner(f)(iter.asIterable(t))
      }
  }

  object syntax {
    implicit class ExtractorOps[T](value: T)(implicit ext: Extractor[T]) {
      def extract(t: PatternFinder[Any]): Seq[t.Pattern] = ext.extract(t)(value)
    }

  }
  def extract[T: Extractor](pf: PatternFinder[Any])(input: T): Seq[pf.Pattern] =
    the[Extractor[T]].extract(pf)(input)

}
