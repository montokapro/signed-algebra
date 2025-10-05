package montokapro.algebra
package instances

import montokapro.algebra.lattice.SignedTree

import algebra.lattice.Bool
import cats.{CommutativeApplicative, Functor, UnorderedTraverse}
import cats.kernel.CommutativeMonoid

package object signedTree extends SignedTreeInstances

trait SignedTreeInstances {
  implicit val signedTreeFunctor: Functor[SignedTree] = new Functor[SignedTree] {
    override def map[A, B](fa: SignedTree[A])(f: A => B) = fa.map(f)
  }

  implicit val signedTreeTraverse: UnorderedTraverse[SignedTree] = new UnorderedTraverse[SignedTree] {
    override def unorderedTraverse[F[_]: CommutativeApplicative, A, B](fa: SignedTree[A])(f: A => F[B]) = fa.unorderedTraverse(f)
    override def unorderedFoldMap[A, B: CommutativeMonoid](fa: SignedTree[A])(f: A => B) = fa.unorderedFoldMap(f)
  }

  implicit def signedTreeBool[A]: Bool[SignedTree[A]] = new SignedTreeBool[A]
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
