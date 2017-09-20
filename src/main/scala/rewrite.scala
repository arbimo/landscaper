import shapeless._
import witness.LiteralWitness

package object rewrite {

  sealed trait Func[FIn, FOut, In] {
    type Result
    def rewrite(f: FIn => FOut, in: In): Result
  }

  trait LowPriority {
    implicit def identity[FIn, FOut, In : LiteralWitness]: Func.Aux[FIn, FOut, In, In] =
      new DefaultFunc[FIn, FOut, In]
  }
  object Func extends LowPriority {
    type Aux[FIn, FOut, In, Result0] = Func[FIn, FOut, In] { type Result = Result0 }

    def apply[FIn, FOut, In](implicit ev: Func[FIn, FOut, In]): Aux[FIn,FOut,In,ev.Result] = ev

    implicit def param[In, Out]: Aux[In, Out, In, Out] = new ParamFunc[In, Out]

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

          (hf.value.rewrite(f, in.head) :: tf.rewrite(f, in.tail).asInstanceOf[TRes2]).asInstanceOf[hf.value.Result :: TRes2]
      }

    implicit def genFunc[FIn,FOut, In, ReprBeforeTrans, ReprAfterTrans](
                                      implicit gen: Generic.Aux[In,ReprBeforeTrans],
                                      rFunc: Func.Aux[FIn, FOut, ReprBeforeTrans, ReprAfterTrans],
                                      ev: ReprBeforeTrans =:= ReprAfterTrans
                                      ): Func.Aux[FIn, FOut, In, In] =
      new Func[FIn, FOut, In] {
        override type Result = In

        override def rewrite(f: (FIn) => FOut, in: In): In =
          gen.from(rFunc.rewrite(f, gen.to(in)).asInstanceOf[ReprBeforeTrans])
      }
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
