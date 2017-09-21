import shapeless._
import witness.LiteralWitness

import scala.collection.generic.CanBuildFrom

package object rewrite {

  sealed trait Func[FIn, FOut, In] {
    type Result
    def rewrite(f: FIn => FOut, in: In): Result
  }

  trait ExtraLowPriority {
    implicit def identity[FIn, FOut, In: LiteralWitness]
      : Func.Aux[FIn, FOut, In, In] =
      new DefaultFunc[FIn, FOut, In]
  }
  trait LowPriority extends ExtraLowPriority {

    implicit def hListFunc[FIn, FOut, H, T <: HList, TRes1, TRes2 <: HList](
        implicit hf: Lazy[Func[FIn, FOut, H]],
        tf: Func.Aux[FIn, FOut, T, TRes1],
        ev: TRes1 =:= TRes2
    ): Func.Aux[FIn, FOut, H :: T, hf.value.Result :: TRes2] =
      new Func[FIn, FOut, H :: T] {
        type Result = hf.value.Result :: TRes2

        // FIXME: we shouldn't have to tell the compiler the types it is handling (perhaps this is due to the =:= constraint
        override def rewrite(f: (FIn) => FOut,
                             in: H :: T): hf.value.Result :: TRes2 =
          (hf.value.rewrite(f, in.head) :: tf
            .rewrite(f, in.tail)
            .asInstanceOf[TRes2]).asInstanceOf[hf.value.Result :: TRes2]
      }

    implicit def coprodFunc[FIn,
                            FOut,
                            H,
                            T <: Coproduct,
                            TRes1,
                            TRes2 <: Coproduct](
        implicit hf: Lazy[Func[FIn, FOut, H]],
        tf: Func.Aux[FIn, FOut, T, TRes1],
        ev: TRes1 =:= TRes2
    ): Func.Aux[FIn, FOut, H :+: T, hf.value.Result :+: TRes2] =
      new Func[FIn, FOut, H :+: T] {
        type Result = hf.value.Result :+: TRes2

        // FIXME: we shouldn't have to tell the compiler the types it is handling (perhaps this is due to the =:= constraint
        override def rewrite(f: (FIn) => FOut,
                             in: H :+: T): hf.value.Result :+: TRes2 =
          in match {
            case Inl(x) =>
              Inl(hf.value.rewrite(f, x).asInstanceOf[hf.value.Result])
            case Inr(x) => Inr(tf.rewrite(f, x).asInstanceOf[TRes2])
          }
      }

    implicit def genFunc[FIn, FOut, In, ReprBeforeTrans, ReprAfterTrans](
        implicit gen: Lazy[Generic.Aux[In, ReprBeforeTrans]],
        rFunc: Func.Aux[FIn, FOut, ReprBeforeTrans, ReprAfterTrans],
        ev: ReprBeforeTrans =:= ReprAfterTrans
    ): Func.Aux[FIn, FOut, In, In] =
      new Func[FIn, FOut, In] {
        override type Result = In

        override def rewrite(f: (FIn) => FOut, in: In): In =
          gen.value.from(
            rFunc.rewrite(f, gen.value.to(in)).asInstanceOf[ReprBeforeTrans])
      }

    implicit def superTypeParam[In, Out, T](
        implicit ev: In <:< T,
        ev2: Out <:< T
    ): Func.Aux[In, Out, T, T] =
      new Func[In, Out, T] {
        override type Result = T

        override def rewrite(f: (In) => Out, in: T): T = in match {
          case x: In => f(x)
          case x: T  => x
        }
      }

    implicit def collRewrite[FIn, FOut, InCol, Repr[_], OutCol, That](
        implicit fInner: Func.Aux[FIn, FOut, InCol, OutCol],
        bf: CanBuildFrom[Repr[InCol], OutCol, That],
        ev: Repr[InCol] <:< scala.collection.generic.FilterMonadic[InCol, Repr[InCol]],
        ev2: That =:= Repr[OutCol]):
      Func.Aux[FIn,FOut,Repr[InCol],That] =
      new Func[FIn,FOut,Repr[InCol]] {
        override type Result = That

        override def rewrite(f: (FIn) => FOut, in: Repr[InCol]): That = {
          val f2: InCol => OutCol = (x: InCol) => fInner.rewrite(f, x)
          in.map(f2)(bf)
        }
      }
  }

  object Func extends LowPriority {
    type Aux[FIn, FOut, In, Result0] = Func[FIn, FOut, In] {
      type Result = Result0
    }

    def apply[FIn, FOut, In](
        implicit ev: Func[FIn, FOut, In]): Aux[FIn, FOut, In, ev.Result] = ev

    implicit def param[In, Out]: Func.Aux[In, Out, In, Out] =
      new ParamFunc[In, Out]

  }

  final class DefaultFunc[FIn, FOut, In] extends Func[FIn, FOut, In] {
    type Result = In
    override def rewrite(f: (FIn) => FOut, in: In): In = in
  }
  final class ParamFunc[In, Out] extends Func[In, Out, In] {
    type Result = Out
    override def rewrite(f: (In) => Out, in: In): Out = f(in)
  }

  def rewrite[FIn, FOut, In](f: FIn => FOut, value: In)(
      implicit func: Func[FIn, FOut, In]): func.Result =
    func.rewrite(f, value)

}
