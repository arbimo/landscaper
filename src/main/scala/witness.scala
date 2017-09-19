import shapeless.HNil

import shapeless._

package object witness {

  trait LiteralWitness[T]
  object LiteralWitness {
    implicit object intWitness extends LiteralWitness[Int]
    implicit object stringWitness extends LiteralWitness[String]
    implicit object doubleWitness extends LiteralWitness[Double]
    implicit object hnilWitness extends LiteralWitness[HNil]
    implicit object cnilWitness extends LiteralWitness[CNil]
  }

  trait IterableWitness[T, Inner] {
    def asIterable(t: T): Iterable[Inner]
  }
  object IterableWitness {
    // default instances (defined with SAM) note that those are subtypes of Iterable
    implicit def seqWitness[T]: IterableWitness[Seq[T], T] = _.toIterable
    implicit def setWitness[T]: IterableWitness[Set[T], T] = _.toIterable
  }

}
