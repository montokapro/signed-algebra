package montokapro.algebra
package instances

import montokapro.algebra.lattice.SignedTree

import algebra.lattice.Bool
import cats.Functor

package object signedTree extends SignedTreeInstances

trait SignedTreeInstances {
  implicit val functor: Functor[SignedTree] = new Functor[SignedTree] {
    override def map[A, B](fa: SignedTree[A])(f: A => B) = fa.map(f)
  }

  implicit def bool[A]: Bool[SignedTree[A]] = new SignedTreeBool[A]
}

class SignedTreeBool[A] extends Bool[SignedTree[A]] {
  override def zero: SignedTree[A] = SignedTree(Set(), Set(one))
  override def one: SignedTree[A] = SignedTree(Set(), Set())

  override def or(x: SignedTree[A], y: SignedTree[A]): SignedTree[A] =
    SignedTree(
      Set(),
      Set(
        SignedTree(
          Set(),
          Set(x, y)
        )
      )
    )

  override def and(x: SignedTree[A], y: SignedTree[A]): SignedTree[A] =
    SignedTree(
      Set(),
      Set(
        SignedTree(
          Set(),
          Set(x)
        ),
        SignedTree(
          Set(),
          Set(y)
        )
      )
    )

  override def complement(x: SignedTree[A]): SignedTree[A] = SignedTree(
    Set(),
    Set(x)
  )
}
