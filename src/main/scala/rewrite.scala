import shapeless._

package object rewrite {

  sealed trait Func[FIn, FOut, In] {
    type Result
    def rewrite(f: FIn => FOut, in: In): Result
  }

  trait LowPriority {
    implicit def identity[FIn, FOut, In]: Func[FIn, FOut, In] =
      new DefaultFunc[FIn, FOut, In]
  }
  object Func extends LowPriority {
    type Aux[FIn, FOut, In, Result] = Func[FIn, FOut, In] { type Result }
    def apply[FIn, FOut, In](implicit ev: Func[FIn, FOut, In]) = ev
    implicit def param[In, Out]: Func[In, Out, In] = new ParamFunc[In, Out]

    implicit def hListFunc[FIn, FOut, H, T <: HList, HRes, TRes <: HList](
                                                                           implicit hf: Lazy[Func.Aux[FIn,FOut,H,HRes]],
                                                                           tf: Func.Aux[FIn, FOut, T, TRes]
                                                                         ): Func[FIn,FOut, H::T] =
      new Func[FIn,FOut,H::T] {
        type Result = HRes :: TRes

        override def rewrite(f: (FIn) => FOut, in: H::T): HRes :: TRes =
          hf.value.rewrite(f, in.head).asInstanceOf[HRes] :: tf.rewrite(f, in.tail).asInstanceOf[TRes]
      }

    def on[FIn, FOut, In](f: FIn => FOut)(value: In)(
      implicit ev: Func[FIn, FOut, In]) =
      ev.rewrite(f, value)
  }

  final class DefaultFunc[FIn, FOut, In] extends Func[FIn, FOut, In] {
    type Result = In
    override def rewrite(f: (FIn) => FOut, in: In): In = in
  }
  final class ParamFunc[In, Out] extends Func[In, Out, In] {
    type Result = Out
    override def rewrite(f: (In) => Out, in: In): Out = f(in)
  }




  def rewrite[FIn, FOut, In, Out, Result](f: FIn => FOut, value: In)(
    implicit func: Func.Aux[FIn, FOut, In, Result], ev: Out =:= Result): Out =
    func.rewrite(f, value).asInstanceOf[Out]

}
