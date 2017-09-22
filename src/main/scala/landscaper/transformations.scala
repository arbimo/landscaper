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

    /** Low priority transformations that are always superseded by the direct transformation when applicable. */
    /** Identity transformation for literal types (including HNil and CNil) */
    implicit def identity[FIn, FOut, In: LiteralWitness]
      : Trans.Aux[FIn, FOut, In, In] =
      new Trans[FIn, FOut, In] {
        type Result = In
        override def rewrite(f: (FIn) => FOut, in: In): In = in
      }

    /** Transformation for HLists */
    implicit def hListTrans[FIn, FOut, H, T <: HList, TRes1, TRes2 <: HList](
        implicit hf: Lazy[Trans[FIn, FOut, H]],
        tf: Trans.Aux[FIn, FOut, T, TRes1],
        ev: TRes1 =:= TRes2
    ): Trans.Aux[FIn, FOut, H :: T, hf.value.Result :: TRes2] =
      new Trans[FIn, FOut, H :: T] {
        type Result = hf.value.Result :: TRes2

        override def rewrite(f: (FIn) => FOut,
                             in: H :: T): hf.value.Result :: TRes2 =
          (hf.value.rewrite(f, in.head) :: tf
            .rewrite(f, in.tail)
            .asInstanceOf[TRes2]).asInstanceOf[hf.value.Result :: TRes2]
      }

    /** Transformation for Coproduct types. */
    implicit def coprodTrans[FIn, FOut, H, T <: Coproduct, TRes <: Coproduct](
        implicit hf: Lazy[Trans[FIn, FOut, H]],
        tf: Lazy[Trans.Aux[FIn, FOut, T, TRes]]
    ): Trans.Aux[FIn, FOut, H :+: T, hf.value.Result :+: TRes] =
      new Trans[FIn, FOut, H :+: T] {
        type Result = hf.value.Result :+: TRes

        override def rewrite(f: (FIn) => FOut,
                             in: H :+: T): hf.value.Result :+: TRes =
          in match {
            case Inl(x) =>
              Inl(hf.value.rewrite(f, x).asInstanceOf[hf.value.Result])
            case Inr(x) => Inr(tf.value.rewrite(f, x))
          }
      }

    /** Transformation for sealed trait and case classes.
      * It relies on the shapeless to find their generic representation.
      * This also works for tuples as long as their type does not change. */
    implicit def genTrans[FIn, FOut, In, Repr](
        implicit gen: Lazy[Generic.Aux[In, Repr]],
        rFunc: Lazy[Trans.Aux[FIn, FOut, Repr, Repr]]
    ): Trans.Aux[FIn, FOut, In, In] =
      new Trans[FIn, FOut, In] {
        override type Result = In

        override def rewrite(f: (FIn) => FOut, in: In): In =
          gen.value.from(
            rFunc.value
              .rewrite(f, gen.value.to(in)))
      }

    /** given f: A => B, provide a transformation T => T if T is a super type of A and B.
      * This is uses runtime reflection to distinguish instances of A */
    implicit def superTypeDirectTrans[In: ClassTag, Out, T](
        implicit ev: In <:< T,
        ev2: Out <:< T
    ): Trans.Aux[In, Out, T, T] =
      new Trans[In, Out, T] {
        val clazz = implicitly[ClassTag[In]].runtimeClass
        override type Result = T

        override def rewrite(f: (In) => Out, in: T): T = in match {
          case x: In if clazz.isInstance(x) => f(x)
          case x                            => x
        }
      }

    /** Provide transformation for scala collection types. */
    implicit def collRewrite[FIn, FOut, InCol, Repr[_], OutCol, That](
        implicit fInner: Trans.Aux[FIn, FOut, InCol, OutCol],
        bf: CanBuildFrom[Repr[InCol], OutCol, That],
        ev: Repr[InCol] <:< scala.collection.generic.FilterMonadic[InCol,
                                                                   Repr[InCol]],
        ev2: That =:= Repr[OutCol]): Trans.Aux[FIn, FOut, Repr[InCol], That] =
      new Trans[FIn, FOut, Repr[InCol]] {
        override type Result = That

        override def rewrite(f: (FIn) => FOut, in: Repr[InCol]): That = {
          val f2: InCol => OutCol = (x: InCol) => fInner.rewrite(f, x)
          in.map(f2)(bf)
        }
      }
  }

  object Trans extends LowPriority {
    type Aux[FIn, FOut, In, Result0] = Trans[FIn, FOut, In] {
      type Result = Result0
    }

    /** Implicitly finds a transformation of "In" by a function "f: FIn => FOut" */
    def apply[FIn, FOut, In](
        implicit ev: Trans[FIn, FOut, In]): Aux[FIn, FOut, In, ev.Result] = ev

    /** Highest priority case: given a function, f: A => B, transform an instance of A into a B. */
    implicit def directTransformation[In, Out]: Trans.Aux[In, Out, In, Out] =
      new Trans[In, Out, In] {
        type Result = Out
        override def rewrite(f: (In) => Out, in: In): Out = f(in)
      }
  }
}
