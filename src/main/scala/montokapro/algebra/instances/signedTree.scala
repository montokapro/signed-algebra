package montokapro.algebra
package instances

import montokapro.algebra.lattice.SignedTree

import algebra.lattice.Bool
import cats.{Applicative, CommutativeApplicative, Eval, Functor, Monad, UnorderedTraverse}
import cats.kernel.CommutativeMonoid
import cats.syntax.all._
import scala.annotation.tailrec

package object signedTree extends SignedTreeInstances

trait SignedTreeInstances {
  implicit val signedTreeTraverse: UnorderedTraverse[SignedTree] = new UnorderedTraverse[SignedTree] {
    override def unorderedTraverse[F[_]: CommutativeApplicative, A, B](fa: SignedTree[A])(f: A => F[B]) = fa.unorderedTraverse(f)
    override def unorderedFoldMap[A, B: CommutativeMonoid](fa: SignedTree[A])(f: A => B) = fa.unorderedFoldMap(f)
  }

  implicit val signedTreeFunctor: Functor[SignedTree] = new Functor[SignedTree] {
    override def map[A, B](fa: SignedTree[A])(f: A => B) = fa.map(f)
  }

  implicit val signedTreeMonad: Monad[SignedTree] = new Monad {
    override def pure[A](a: A): SignedTree[A] = SignedTree(Set(a), Set())

    override def flatMap[A, B](fa: SignedTree[A])(f: A => SignedTree[B]): SignedTree[B] = fa.flatMap(f)

    // Consider trampolining
    override def tailRecM[A, B](a: A)(f: A => SignedTree[Either[A, B]]): SignedTree[B] = {
      flatMap(f(a)) {
        case Left(value) =>
          tailRecM(value)(f)
        case Right(value) =>
          pure(value)
      }
    }
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
