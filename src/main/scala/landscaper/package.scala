import landscaper.eraser.{Eraser, PartialPredicate, Predicate}
import landscaper.extractor.{Extractor, PatternFinder}
import landscaper.transformations.Trans
import shapeless.the

package object landscaper {

  // ========= General rewriting of mixed ADT and collections ==========

  /** Transforms a data structure "d" using a function that is applied on all constituent of "d"
    *
    * @param f Function to apply recursively on the data structure.
    * @param d Data structure to transform.
    * @param func implicit representation of the transformation at the type level.
    * @return
    */
  def rewrite[FIn, FOut, In](f: FIn => FOut, d: In)(
      implicit func: Trans[FIn, FOut, In]): func.Result =
    func.rewrite(f, d)


  // ======== Pattern Extraction =========

  /** Constructs a pattern finder from a partial functions.
    *
    * usage:
    * {{{
    *   // build a pattern for extracting all strings starting with "A"
    *   pattern { case x: String if x.startsWith("A") => Seq(x) }
    * }}}
    *
    */
  def pattern[Out](
      pf: PartialFunction[Any, Seq[Out]]): PatternFinder.Aux[Any, Out] =
    PatternFinder(pf)

  /** Extracts all instances of a pattern in a given data type. */
  def extract[T: Extractor](pf: PatternFinder[Any], input: T): Seq[pf.Pattern] =
    the[Extractor[T]].extract(pf)(input)


  // ======== Removal from nested collections ========

  /** Removes any instance matches by the predicate appearing in "t". */
  def erase[T : Eraser](pred: Any => Boolean)(t: T): T =
    Eraser[T].erase(pred)(t)

  /** Removes any instance matches by the predicate appearing in "t". */
  def erase[T: Eraser](pred: PartialFunction[Any,Boolean])(t: T): T =
    Eraser[T].erase(pred)(t)

}
