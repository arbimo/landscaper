package landscaper

import landscaper.witness.LiteralWitness
import shapeless._

object eraser {

  type PartialPredicate = PartialFunction[Any, Boolean]
  type Predicate = (Any => Boolean)

  def predicate(ppred: PartialPredicate): Predicate =
    x => if (ppred.isDefinedAt(x)) ppred(x) else false

  trait Eraser[T] {
    def erase(pred: Predicate)(t: T): T
    final def erase(ppred: PartialPredicate)(t: T): T =
      erase(predicate(ppred))(t)
  }

  object Eraser {
    def apply[T](implicit er: Eraser[T]): Eraser[T] = er
    def instance[T](f: (Predicate, T) => T): Eraser[T] =
      new Eraser[T] {
        override def erase(pred: Predicate)(t: T): T = f(pred, t)
      }

    // literal are not touched
    implicit def literalEraser[T: LiteralWitness] = new Eraser[T] {
      override def erase(pred: (Any) => Boolean)(t: T): T = t
    }

    implicit def seqEraser[Content, T <: Seq[Content]](
        implicit er: Eraser[Content]): Eraser[Seq[Content]] =
      instance[Seq[Content]](
        (pred: Predicate, t: Seq[Content]) =>
          t.map(er.erase(pred))
            .filterNot(pred))

    implicit def genEraser[T, R](
        implicit gen: Generic.Aux[T, R],
        rEr: Lazy[Eraser[R]]
    ): Eraser[T] =
      instance(
        (pred: Predicate, t: T) => gen.from(rEr.value.erase(pred)(gen.to(t))))

    implicit def hListEraser[H, T <: HList](
        implicit hEr: Lazy[Eraser[H]],
        tEr: Eraser[T]
    ): Eraser[H :: T] =
      instance[H :: T]((p: Predicate, l: H :: T) =>
        hEr.value.erase(p)(l.head) :: tEr.erase(p)(l.tail))

    implicit def coprodEraser[H, T <:Coproduct](
                                               implicit hEr: Lazy[Eraser[H]],
                                               tEr: Eraser[T]
                                               ): Eraser[H :+: T] = instance((p: Predicate, c: H :+: T) =>
      c match {
        case Inl(x) => Inl(hEr.value.erase(p)(x))
        case Inr(x) => Inr(tEr.erase(p)(x))
      }
    )
  }

  def erase[T : Eraser](pred: Predicate)(t: T): T = Eraser[T].erase(pred)(t)
  def erase[T: Eraser](pred: PartialPredicate)(t: T): T =
    Eraser[T].erase(pred)(t)
}
