package landscaper

import shapeless._

object witness {

  /** Type class use to declare a given type as a literal.
    * Literal types are used to stop the recursion when inspect the content of objects.
    */
  trait LiteralWitness[T]
  object LiteralWitness {
    implicit val intWitness = literal[Int]
    implicit val stringWitness = literal[String]
    implicit val doubleWitness = literal[Double]
    implicit val hnilWitness = literal[HNil]
    implicit val hnilTypeWitness = literal[HNil.type]
    implicit val cnilWitness = literal[CNil]
  }

  /** Creates a new literal witness for the given type. */
  def literal[T] = new LiteralWitness[T] {}
}
