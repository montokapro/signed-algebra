package montokapro.algebra
package lattice

import montokapro.algebra.instances.signed._

import algebra.instances.set._
import algebra.lattice.{Bool, GenBool}

case class SignedTree[A](positives: Set[A], negatives: Set[SignedTree[A]]) {
  // Consider trampolining
  def reduce(): Signed[Set[A]] = {
    // scala 2 compilation hint
    val setGenBool: GenBool[Set[A]] = GenBool[Set[A]]
    val signedSetBool: Bool[Signed[Set[A]]] = signedBool(setGenBool)

    import signedSetBool._

    val set = Signed[Set[A]](false, positives)
    imp(meetSemilattice.combineAll(negatives.map(_.reduce())), set)
  }

  def map[B](f: A => B): SignedTree[B] = {
    SignedTree(positives.map(f), negatives.map(_.map(f)))
  }
}
