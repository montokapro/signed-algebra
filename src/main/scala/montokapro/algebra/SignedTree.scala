package montokapro.algebra

import montokapro.algebra.instances.signed._

import algebra.instances.set._
import algebra.lattice.{Bool, GenBool}
import cats.{CommutativeApplicative, Eval}
import cats.kernel.CommutativeMonoid
import cats.syntax.all._

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

  def map[B](f: A => B) = {
    def go(tree: SignedTree[A]): Eval[SignedTree[B]] = {
      tree.negatives
        .unorderedTraverse(go)
        .map(n => SignedTree(tree.positives.map(f), n))
    }

    go(this).value
  }

  def inverseFlatMap[B](f: A => SignedTree[B]): SignedTree[B] = {
    def go(tree: SignedTree[A]): Eval[SignedTree[B]] = {
      tree.negatives
        .unorderedTraverse(go)
        .map(n => SignedTree(Set(), tree.positives.map(f) ++ n))
    }

    go(this).value
  }

  def flatMap[B](f: A => SignedTree[B]): SignedTree[B] = {
    inverseFlatMap(a => SignedTree(Set(), Set(f(a))))
  }

  // Consider trampolining
  def unorderedTraverse[F[_], B](
    f: A => F[B]
  )(implicit applicative: CommutativeApplicative[F]): F[SignedTree[B]] = {
    val fPositive: F[Set[B]] = applicative.pure(Set.empty[B])
    val fPositives: F[Set[B]] = positives
      .foldRight(fPositive) { (a: A, acc: F[Set[B]]) =>
        val fb: F[B] = f(a)
        applicative.map2(acc, fb)(_ + _)
      }

    val fNegative: F[Set[SignedTree[B]]] =
      applicative.pure(Set.empty[SignedTree[B]])
    val fNegatives: F[Set[SignedTree[B]]] = negatives
      .foldRight(fNegative) { (a: SignedTree[A], acc: F[Set[SignedTree[B]]]) =>
        val fb: F[SignedTree[B]] = a.unorderedTraverse(f)(applicative)
        applicative.map2(acc, fb)(_ + _)
      }

    applicative.map2(fPositives, fNegatives)(SignedTree.apply)
  }

  // Consider trampolining
  def unorderedFoldMap[B](
    f: A => B
  )(implicit monoid: CommutativeMonoid[B]): B = {
    negatives.foldRight(
      positives.foldRight(monoid.empty)((a, b) => monoid.combine(f(a), b))
    )((tree: SignedTree[A], b) =>
      monoid.combine(tree.unorderedFoldMap(f)(monoid), b)
    )
  }
}
