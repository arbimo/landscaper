import shapeless.{HList, HNil}
import shapeless.PolyDefns.->

object Rewrite extends App {

  sealed trait Node
  case class Named(name: String, node: Literal) extends Node
  case class Literal(content: String) extends Node

  val data: Seq[Node] =
    Seq(Literal("X"), Named("first", Literal("X")), Literal("X"), Literal("Y"))

  trait PatternExtractor[Input, Pattern] {
    def extract(in: Input): Option[Pattern]
  }
  val extractor = new PatternExtractor[Node, Named] {
    override def extract(in: Node): Option[Named] = in match {
      case x: Named => Some(x)
      case _        => None
    }
  }

  def scan[In, Out](in: Seq[In],
                    extractor: PatternExtractor[In, Out]): Seq[Out] =
    in.map(extractor.extract)
      .foldLeft(Seq[Out]())((seq, opt) => seq ++ opt.toSeq)

  val patterns = scan(data, extractor)

  // generates a poly1 from a function2
  def rewrite(pattern: Named)(n: Literal): Literal = {
    println(s"$pattern  --  $n")
    (pattern, n) match {
      case (Named(name, Literal(x)), Literal(y)) if x == y =>
        Literal(name + ":" + x)
      case (_, x) => x
    }
  }

  trait Rule[Pattern, In, Out] {
    def rule(pat: Pattern, in: In): Out
  }

  import shapeless._

  val polys: Seq[->[Literal, Literal]] =
    patterns.map(pattern => new ->(rewrite(pattern)))
  object p extends ->(rewrite(patterns.head))

  println(everywhere(p)(data))
  class TranfromFromPattern[Pattern, In, Out](pattern: Pattern,
                                              f: Pattern => In => Out)
      extends ->[In, Out](f(pattern))

  object transPat extends TranfromFromPattern(patterns.head, rewrite)
  def transPatDef[Pattern, In, Out](pattern: Pattern,
                                    f: Pattern => In => Out): ->[In, Out] = {
    new ->(f(pattern))
  }
  val transPatVal = new Poly1 {
    implicit def trans[Node] = at(rewrite(patterns.head))
  }
  println("-- OBJ --")
  println(everywhere(transPat)(data))
  println("-- VAL --")
  println(everywhere(transPatVal)(data))
  println("-- DEF --")
  println(everywhere(transPatDef(patterns.head, rewrite))(data))

  // fold: (current, ACC) where ACC = (Context, Results)
  object toRecord extends Poly2 {
    implicit def caseNode =
      at[Node, (PatternExtractor[Node, Named], Seq[Named])] {
        case (curr, (extractor, acc)) =>
          (extractor, acc ++ extractor.extract(curr).toList)
      }
  }
//  def extract(extractor: PatternExtractor[Node,Named], data: Seq[Node]): Seq[Named] =
//    everything(toRecord)(data)
  // everywhere will not be applied inside a type that is already matched by the Poly
  // apparently the Poly should be defined as object or any stable identifier (not created with new so that the compiler can find
  // the implicit def, otherwise everywhere always the default (identity) function
}

object PrintTraverse extends App {
  import shapeless._
  sealed trait Node
  case class Named(name: String, node: Literal) extends Node
  case class Literal(content: String) extends Node

  trait Traverser[T] {
    def traverse(t: T): Unit
  }
  object Traverser {
    def apply[T](implicit trav: Traverser[T]): Traverser[T] = trav
    def instance[T](f: T => Unit): Traverser[T] = new Traverser[T] {
      override def traverse(t: T) { f(t) }
    }
    implicit def traverseString = instance[String](s => println("string: " + s))
    implicit def traverseInt = instance[Int](i => println("int: " + i))
    implicit def traverseHNil = instance[HNil](x => {})
    implicit def traverseHList[H, T <: HList](
        implicit hTrav: Traverser[H],
        tTrav: Traverser[T]
    ): Traverser[H :: T] =
      instance[H :: T]((l: H :: T) => {
        hTrav.traverse(l.head); tTrav.traverse(l.tail)
      })

    implicit def traverseCNil = instance[CNil](x => {})
    implicit def traverseCoprod[H, T <: Coproduct](
        implicit hTrav: Traverser[H],
        tTrav: Traverser[T]
    ): Traverser[H :+: T] =
      instance(_ match {
        case Inl(x) => hTrav.traverse(x)
        case Inr(x) => tTrav.traverse(x)
      })

    implicit def traverseADT[T, R](
        implicit gen: Generic.Aux[T, R],
        trav: Traverser[R]
    ): Traverser[T] =
      instance[T]((t: T) => trav.traverse(gen.to(t)))

//    implicit def traverseCoprod[T, R](
//                                     implicit gen: Generic.Aux[T,R]
//                                     )
  }

  Traverser[String :: Int :: HNil].traverse("Coucou" :: 1 :: HNil)
  Traverser[Node].traverse(Named("hello-in-english", Literal("hi")))
}

object GenTraverse extends App {
  import shapeless._
  sealed trait Node
  case class Named(name: String, node: Literal) extends Node
  case class Literal(content: String) extends Node

  trait Traverser[T] {
    def traverse(t: T, f: Any => Unit): Unit
  }
  object Traverser {
    def apply[T](implicit trav: Traverser[T]): Traverser[T] = trav
    def terminal[T]: Traverser[T] = new Traverser[T] {
      override def traverse(t: T, comp: Any => Unit) { }
    }
    // terminal nodes on which to end recursive lookup
    implicit def traverseString = terminal[String]
    implicit def traverseInt = terminal[Int]
    implicit def traverseHNil = terminal[HNil]
    implicit def traverseCNil = terminal[CNil]

    implicit def traverseHList[H, T <: HList](
        implicit hTrav: Traverser[H],
        tTrav: Traverser[T]
    ): Traverser[H :: T] = new Traverser[H::T] {
        override def traverse(t: ::[H, T], f: (Any) => Unit): Unit = {
          f(t.head)
          hTrav.traverse(t.head, f)
          tTrav.traverse(t.tail, f)
        }
    }
    implicit def traverseCoprod[H, T <: Coproduct](
        implicit hTrav: Traverser[H],
        tTrav: Traverser[T]
    ): Traverser[H :+: T] = new Traverser[H :+: T] {
      override def traverse(t: :+:[H, T], f: (Any) => Unit): Unit = t match {
        case Inl(x) =>
          f(x)
          hTrav.traverse(x, f)
        case Inr(x) => tTrav.traverse(x, f)
      }
    }

    implicit def traverseADT[T, R](
        implicit gen: Generic.Aux[T, R],
        trav: Traverser[R]
    ): Traverser[T] = new Traverser[T] {
      override def traverse(t: T, f: (Any) => Unit): Unit = trav.traverse(gen.to(t), f)
    }
  }

  Traverser[String :: Int :: HNil].traverse("Coucou" :: 1 :: HNil, println)
  Traverser[Node].traverse(Named("hello-in-english", Literal("hi")), println)
}
