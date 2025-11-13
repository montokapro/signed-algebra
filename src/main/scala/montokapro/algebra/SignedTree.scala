package montokapro.algebra

import montokapro.algebra.instances.signed._

import algebra.instances.set._
import algebra.lattice.{Bool, GenBool}
import cats.{Applicative, CommutativeApplicative, Eval, StackSafeMonad}
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

  def map[B](f: A => B): SignedTree[B] = {
    def go(tree: SignedTree[A]): Eval[SignedTree[B]] = {
      tree.negatives
        .unorderedTraverse(go)
        .map(t => SignedTree(tree.positives.map(f), t))
    }

    go(this).value
  }

  def inverseFlatMap[B](f: A => SignedTree[B]): SignedTree[B] = {
    def go(tree: SignedTree[A]): Eval[SignedTree[B]] = {
      tree.negatives
        .unorderedTraverse(go)
        .map(t => SignedTree(Set(), tree.positives.map(f) ++ t))
    }

    go(this).value
  }

  def flatMap[B](f: A => SignedTree[B]): SignedTree[B] = {
    inverseFlatMap(a => SignedTree(Set(), Set(f(a))))
  }

  def unorderedTraverseDirectly[F[_], B](
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
  def unorderedTraverse[F[_], B](
    f: A => F[B]
  )(implicit applicative: CommutativeApplicative[F]): F[SignedTree[B]] =
    applicative match {
      case m: StackSafeMonad[F] => unorderedTraverseDirectly(f)
      case _ => {
        val evalApplicative: CommutativeApplicative[Eval] =
          CommutativeApplicative[Eval]

        // Sadly, compose only returns an Applicative
        val composedApplicative: Applicative[({ type L[Z] = Eval[F[Z]] })#L] =
          evalApplicative.compose(applicative)

        val r: Eval[F[SignedTree[B]]] =
          unorderedTraverseDirectly[({ type L[Z] = Eval[F[Z]] })#L, B](
            f.andThen(evalApplicative.pure)
          )(
            new CommutativeApplicative[({ type L[Z] = Eval[F[Z]] })#L] {
              def pure[A](a: A): Eval[F[A]] = composedApplicative.pure(a)
              def ap[A, B](ff: Eval[F[A => B]])(fa: Eval[F[A]]): Eval[F[B]] =
                composedApplicative.ap(ff)(fa)
            }
          )
        r.value
      }
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
