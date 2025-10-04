package montokapro.algebra
package lattice

import montokapro.algebra.instances.signed._

import algebra.instances.set._
import algebra.lattice.{Bool, GenBool}

case class SignedTree[A](positives: Set[A], negatives: Set[SignedTree[A]]) {
  // Consider trampolining
  def reduce(): Signed[Set[A]] = {
    // TODO: fails to compile for scala 2
    val bool = Bool[Signed[Set[A]]]
    val set = Signed[Set[A]](false, positives)
    bool.imp(bool.meetSemilattice.combineAll(negatives.map(_.reduce())), set)
  }
}
