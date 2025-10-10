package montokapro.algebra
package instances

import montokapro.algebra.SignedTree

import algebra.lattice.Bool
import cats.{
  Applicative,
  CommutativeApplicative,
  Eval,
  Functor,
  Monad,
  UnorderedTraverse
}
import cats.kernel.CommutativeMonoid
import cats.syntax.all._
import io.circe._
import io.circe.syntax._

package object signedTree extends SignedTreeInstances

trait SignedTreeInstances {
  implicit val signedTreeTraverse: UnorderedTraverse[SignedTree] =
    new UnorderedTraverse[SignedTree] {
      override def unorderedTraverse[F[_]: CommutativeApplicative, A, B](
        fa: SignedTree[A]
      )(f: A => F[B]) = fa.unorderedTraverse(f)
      override def unorderedFoldMap[A, B: CommutativeMonoid](fa: SignedTree[A])(
        f: A => B
      ) = fa.unorderedFoldMap(f)
    }

  implicit val signedTreeFunctor: Functor[SignedTree] =
    new Functor[SignedTree] {
      override def map[A, B](fa: SignedTree[A])(f: A => B) = fa.map(f)
    }

  implicit val signedTreeMonad: Monad[SignedTree] = new Monad[SignedTree] {
    override def pure[A](a: A): SignedTree[A] = SignedTree(Set(a), Set())

    override def flatMap[A, B](fa: SignedTree[A])(
      f: A => SignedTree[B]
    ): SignedTree[B] = fa.flatMap(f)

    // Consider trampolining
    override def tailRecM[A, B](
      a: A
    )(f: A => SignedTree[Either[A, B]]): SignedTree[B] = {
      flatMap(f(a)) {
        case Left(value) =>
          tailRecM(value)(f)
        case Right(value) =>
          pure(value)
      }
    }
  }

  implicit def signedTreeBool[A]: Bool[SignedTree[A]] = new SignedTreeBool[A]

  // https://circe.github.io/circe/codecs/recursive-adt.html
  implicit def signedTreeDecoder[A: Decoder]: Decoder[SignedTree[A]] =
    Decoder.recursive(recurse => new SignedTreeDecoder[A](recurse))
  implicit def signedTreeEncoder[A: Encoder]: Encoder[SignedTree[A]] =
    Encoder.recursive(recurse => new SignedTreeEncoder[A](recurse))
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

class SignedTreeEncoder[A](self: Encoder[SignedTree[A]])(implicit
  encoder: Encoder[A]
) extends Encoder[SignedTree[A]] {
  override def apply(signedTree: SignedTree[A]): Json = Set
    .concat(
      signedTree.positives.map(_.asJson(encoder)),
      signedTree.negatives.map(_.asJson(self))
    )
    .asJson
}

class SignedTreeDecoder[A](self: Decoder[SignedTree[A]])(implicit
  decoder: Decoder[A]
) extends Decoder[SignedTree[A]] {
  implicit val elementDecoder: Decoder[Either[A, SignedTree[A]]] =
    decoder.either(self)

  override def apply(c: HCursor): Decoder.Result[SignedTree[A]] = {
    c.as[Vector[Json]]
      .flatMap(jsons => {
        val unit: SignedTree[A] = SignedTree[A](Set(), Set())

        def go(
          acc: SignedTree[A],
          index: Int
        ): Decoder.Result[SignedTree[A]] = {
          c.downN(index).as[Either[A, SignedTree[A]]](elementDecoder).map {
            case Left(p) => SignedTree(acc.positives + p, acc.negatives)
            case Right(n) => SignedTree(acc.positives, acc.negatives + n)
          }
        }

        jsons.indices.toList.foldM(unit)(go)
      })
  }
}
