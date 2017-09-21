package landscaper

import shapeless._

object witness {

  trait LiteralWitness[T]
  object LiteralWitness {
    implicit object intWitness extends LiteralWitness[Int]
    implicit object stringWitness extends LiteralWitness[String]
    implicit object doubleWitness extends LiteralWitness[Double]
    implicit object hnilWitness extends LiteralWitness[HNil]
    implicit object hnilTypeWitness extends LiteralWitness[HNil.type]
    implicit object cnilWitness extends LiteralWitness[CNil]
  }
}
