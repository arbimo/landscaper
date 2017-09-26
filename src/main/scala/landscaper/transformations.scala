package landscaper

import landscaper.witness.LiteralWitness
import shapeless._

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

object transformations {

  /**
    * Represents a transformation of some type In by a function f: FIn => FOut
    * @tparam FIn Input type of the function
    * @tparam FOut Output type of the function
    * @tparam In Type on which to apply the transformation.
    */
  sealed trait Trans[FIn, FOut, In] {
    type Result
    def rewrite(f: FIn => FOut, in: In): Result
  }

  trait LowPriority {

    /** Highest priority case: given a function, f: A => B, transform an instance of A into a B. */
    implicit def directTransformation[FIn, FOut, In](
        implicit ev: In <:< FIn): Trans.Aux[FIn, FOut, In, FOut] =
      new Trans[FIn, FOut, In] {
        type Result = FOut
        override def rewrite(f: FIn => FOut, in: In): FOut = f(in)
      }

    /** Low priority transformations that are always superseded by the direct transformation when applicable. */
    /** Identity transformation for literal types (including HNil and CNil) */
    implicit def identity[FIn, FOut, In: LiteralWitness](
        implicit ev: FIn <:!< In): Trans.Aux[FIn, FOut, In, In] =
      new Trans[FIn, FOut, In] {
        type Result = In
        override def rewrite(f: (FIn) => FOut, in: In): In = in
      }
  }

  object Trans extends LowPriority {
    type Aux[FIn, FOut, In, Result0] = Trans[FIn, FOut, In] {
      type Result = Result0
    }

    /** Implicitly finds a transformation of "In" by a function "f: FIn => FOut" */
    def apply[FIn, FOut, In](implicit ev: Trans[FIn, FOut, In]): Aux[FIn, FOut, In, ev.Result] = ev

    /** Transformation for HLists */
    implicit def hListTrans[FIn, FOut, H, T <: HList, HRes, TRes <: HList](
        implicit hf: Lazy[Trans.Aux[FIn, FOut, H, HRes]],
        tf: Lazy[Trans.Aux[FIn, FOut, T, TRes]]
    ): Trans.Aux[FIn, FOut, H :: T, HRes :: TRes] =
      new Trans[FIn, FOut, H :: T] {
        type Result = HRes :: TRes

        override def rewrite(f: (FIn) => FOut, in: H :: T): HRes :: TRes =
          hf.value.rewrite(f, in.head) :: tf.value
            .rewrite(f, in.tail)
      }

    /** Transformation for Coproduct types. */
    implicit def coprodTrans[FIn, FOut, H, T <: Coproduct, HRes, TRes <: Coproduct](
        implicit hf: Lazy[Trans.Aux[FIn, FOut, H, HRes]],
        tf: Lazy[Trans.Aux[FIn, FOut, T, TRes]],
        ev: FIn =:!= HRes
    ): Trans.Aux[FIn, FOut, H :+: T, HRes :+: TRes] =
      new Trans[FIn, FOut, H :+: T] {
        type Result = HRes :+: TRes

        override def rewrite(f: (FIn) => FOut, in: H :+: T): HRes :+: TRes =
          in match {
            case Inl(x) => Inl(hf.value.rewrite(f, x))
            case Inr(x) => Inr(tf.value.rewrite(f, x))
          }
      }

    /** Transformation for Coproduct types. */
    implicit def coprodDirectTrans[FIn, FOut, H, T <: Coproduct, HRes, TRes <: Coproduct](
        implicit hf: Lazy[Trans.Aux[FIn, FOut, H, HRes]],
        tf: Lazy[Trans.Aux[FIn, FOut, T, TRes]],
        ev: FIn =:= HRes
    ): Trans.Aux[FIn, FOut, H :+: T, FOut :+: TRes] =
      new Trans[FIn, FOut, H :+: T] {
        type Result = FOut :+: TRes

        override def rewrite(f: (FIn) => FOut, in: H :+: T): FOut :+: TRes =
          in match {
            case Inl(x) => Inl(f(hf.value.rewrite(f, x).asInstanceOf[FIn]))
            case Inr(x) => Inr(tf.value.rewrite(f, x))
          }
      }

    /** Transformation for sealed trait and case classes on which the transformation is not directly applicable. */
    implicit def genTransNonApplicable[FIn, FOut, In, Repr](
        implicit gen: Lazy[Generic.Aux[In, Repr]],
        rFunc: Lazy[Trans.Aux[FIn, FOut, Repr, Repr]],
        ev: In <:!< FIn
    ): Trans.Aux[FIn, FOut, In, In] =
      new Trans[FIn, FOut, In] {
        override type Result = In

        override def rewrite(f: (FIn) => FOut, in: In): In =
          gen.value.from(
            rFunc.value
              .rewrite(f, gen.value.to(in)))
      }

    /** Transformation for sealed trait and case classes on which the transformation is directly applicable. */
    implicit def genTransApplicable[FIn, FOut, In, Repr](
        implicit gen: Lazy[Generic.Aux[In, Repr]],
        rFunc: Lazy[Trans.Aux[FIn, FOut, Repr, Repr]],
        ev: In <:< FIn
    ): Trans.Aux[FIn, FOut, In, FOut] =
      new Trans[FIn, FOut, In] {
        override type Result = FOut

        override def rewrite(f: (FIn) => FOut, in: In): FOut =
          f(
            gen.value.from(rFunc.value
              .rewrite(f, gen.value.to(in))))
      }

    /** Provide transformation for scala collection types. */
    implicit def collRewrite[FIn, FOut, InCol, Repr[_], OutCol, That](
        implicit fInner: Trans.Aux[FIn, FOut, InCol, OutCol],
        bf: CanBuildFrom[Repr[InCol], OutCol, That],
        ev: Repr[InCol] <:< scala.collection.generic.FilterMonadic[InCol, Repr[InCol]],
        ev2: That =:= Repr[OutCol]): Trans.Aux[FIn, FOut, Repr[InCol], That] =
      new Trans[FIn, FOut, Repr[InCol]] {
        override type Result = That

        override def rewrite(f: (FIn) => FOut, in: Repr[InCol]): That = {
          val f2: InCol => OutCol = (x: InCol) => fInner.rewrite(f, x)
          in.map(f2)(bf)
        }
      }
  }
}
