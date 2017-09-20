import witness.LiteralWitness

object PolyTest extends App {
  import shapeless._

  trait IdentityByDefault extends Poly1 {
    implicit def anyCase[T]: Case.Aux[T, T] = at[T](t => t)
  }

  object pol extends Poly1 with IdentityByDefault {
    implicit val intCase: Case.Aux[Int, String] = at[Int](i => "int: " + i)
  }
//  object identity extends Poly1 {
//    implicit def anyCase[T]: Case.Aux[T,T] = at[T](t => t)
//  }

  object pol2 extends Poly2 {
    implicit val intCase: Case.Aux[Int => String, Int, String] = at(
      (f: Int => String, v: Int) => f(v))
  }

  val i = 42
  val asStr = (i: Int) => "int: " + i

  println(pol2(asStr, 1))

  val funcs: Seq[Int => String] = Seq("int1: " + _, "int2: " + _)

  for (f <- funcs) {
    println(pol2(f, 1))
  }

  trait IdentityPoly2 extends Poly2 {
//    implicit def idCase[T,In,Out]: Case.Aux[In=>Out,T,T] = at((f: In => Out, v: T) => v)
    implicit def idStringCase[In, Out]: Case.Aux[In => Out, String, String] =
      at((_: In => Out, v: String) => v)
  }

  trait Swapper[In, Out] {
    object poly extends Poly2 with IdentityPoly2 {
//      implicit val myCase: Case.Aux[In => Out, In, Out] = at(
//        (f: In => Out, v: In) => f(v))

      implicit val intCase: Case.Aux[Int => String, Int, String] = at(
        (f: Int => String, v: Int) => f(v))
    }
    // should have a poly2 instance
    def replace(f: In => Out)(in: In): Out
//    def replaceIn[T](f: In => Out)(in: Int): Any
  }
  trait MyPoly[In, Out, T] extends Poly2 {}
  object Swapper {
    def apply[In, Out](implicit ev: Swapper[In, Out]) = ev
//    implicit def stringIntSwapper: Swapper[Int,String] = new Swapper[Int,String] {
//      override def replace(f: Int => String)(in: Int): String = poly(f, in)
//      def replaceIn[T <: Int](f: Int => String)(in: Int) = poly[Int=>String,Int](f, in: Int)(poly.intCase)
//    }
  }
  object StringIntSwapper extends Swapper[Int, String] {
    override def replace(f: Int => String)(in: Int): String = poly(f, in)
    def replaceIn[In](f: Int => String)(in: In)(implicit ev: In =:= Int) =
      poly(f, in: Int)
  }

  object poly extends Poly2 with IdentityPoly2 {
    implicit val myCase: Case.Aux[Int => String, Int, String] = at(
      (f: Int => String, v: Int) => f(v))
  }

//  println(poly(asStr, "coucou"))

  println(StringIntSwapper.replaceIn(asStr)(42))
  val x = StringIntSwapper
  println(StringIntSwapper.poly.use((asStr, "")))
}

object Poly1Test extends App {
  import shapeless._

  sealed trait Func[FIn, FOut, In] {
    type Result
    def compute(f: FIn => FOut, in: In): Result
  }

  trait LowPriority {
    implicit def identity[FIn, FOut, In]: Func[FIn, FOut, In] =
      new DefaultFunc[FIn, FOut, In]
  }
  object Func extends LowPriority {
    type Aux[FIn, FOut, In, Result] = Func[FIn, FOut, In] { type Result }
    def apply[FIn, FOut, In](implicit ev: Func[FIn, FOut, In]) = ev
    implicit def param[In, Out]: Func[In, Out, In] = new ParamFunc[In, Out]

    implicit def hListLast[FIn, FOut, H](implicit ev: Func[FIn,FOut,H]): Func[FIn,FOut, H::HNil] =
      new Func[FIn,FOut,H::HNil] {
        type Result = ev.Result :: HNil

        override def compute(f: (FIn) => FOut, in: ::[H, HNil]): ::[ev.Result, HNil] =
          ev.compute(f, in.head) :: HNil
      }

    def on[FIn, FOut, In](f: FIn => FOut)(value: In)(
        implicit ev: Func[FIn, FOut, In]) =
      ev.compute(f, value)
  }

  final class DefaultFunc[FIn, FOut, In] extends Func[FIn, FOut, In] {
    type Result = In
    override def compute(f: (FIn) => FOut, in: In): In = in
  }
  final class ParamFunc[In, Out] extends Func[In, Out, In] {
    type Result = Out
    override def compute(f: (In) => Out, in: In): Out = f(in)
  }

  val f = (x: Int) => "int: " + x

  println(Func[Int, Int, String])
  def applyOn[FIn, FOut, In](f: FIn => FOut, value: In)(
      implicit func: Func[FIn, FOut, In]): func.Result =
    func.compute(f, value)

  println(applyOn(f, "A"))
  println(applyOn(f, 1::HNil))
}
