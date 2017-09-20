import shapeless._

package object rewrite {

  sealed trait Func[FIn, FOut, In] {
    type Result
    def rewrite(f: FIn => FOut, in: In): Result
  }

  trait LowPriority {
    implicit def identity[FIn, FOut, In]: Func.Aux[FIn, FOut, In, In] =
      new DefaultFunc[FIn, FOut, In]
  }
  object Func extends LowPriority {
    type Aux[FIn, FOut, In, Result0] = Func[FIn, FOut, In] { type Result = Result0 }

    def apply[FIn, FOut, In](implicit ev: Func[FIn, FOut, In]): Aux[FIn,FOut,In,ev.Result] = ev

    implicit def param[In, Out]: Aux[In, Out, In, Out] = new ParamFunc[In, Out]

//    implicit def hListFunc[FIn, FOut, H, T <: HList](
//        implicit hf: Lazy[Func[FIn, FOut, H]],
//        tf: Func[FIn, FOut, T]
//    ): Func.Aux[FIn, FOut, H :: T, hf.value.Result :: tf.Result] =
//      new Func[FIn, FOut, H :: T] {
//        type Result = hf.value.Result :: tf.Result
//
//        override def rewrite(f: (FIn) => FOut,
//                             in: H :: T): hf.value.Result :: tf.Result =
//          hf.value.rewrite(f, in.head) :: tf.rewrite(f, in.tail)
//      }
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
