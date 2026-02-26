package montokapro.algebra
package instances

import montokapro.algebra.SignedTree
import montokapro.algebra.instances.signed._

import algebra.instances.set._
import algebra.lattice.{Bool, GenBool}
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

package object tree extends TreeInstances

trait TreeInstances {
  implicit class TreeOps[A](tree: Tree[A]) {
    def toSignedSet(): Signed[Set[A]] = {
      val setGenBool: GenBool[Set[A]] = GenBool[Set[A]]
      val signedSetBool: Bool[Signed[Set[A]]] = signedBool(setGenBool)
      import signedSetBool._

      tree match {
        case Leaf(value) =>
          Signed[Set[A]](false, Set(value))
        case Branch(set) =>
          meetSemilattice.combineAll(
            set.map(_.toSignedSet().flatMap(Signed(true, _)))
          )
      }
    }

    def reduce(): Tree[A] = Tree.fromSignedSet(toSignedSet())
  }

  implicit class SetTreeOps[A](tree: Tree[Set[A]]) {
    def toFlatTree(): Tree[A] = {
      tree.flatMap(set => Branch(set.map(Leaf(_))))
    }

    def toFlatSignedSet(): Signed[Set[A]] = {
      val setGenBool: GenBool[Set[A]] = GenBool[Set[A]]
      val signedSetBool: Bool[Signed[Set[A]]] = signedBool(setGenBool)
      import signedSetBool._

      tree match {
        case Leaf(set) =>
          Signed(true, set)
        case Branch(s) =>
          meetSemilattice.combineAll(
            s.map(_.toFlatSignedSet().flatMap(Signed(true, _)))
          )
      }
    }

    def flatReduce(): Tree[Set[A]] = Tree.fromFlatSignedSet(toFlatSignedSet())
  }

  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
        def go(x: Tree[A]): Eval[Tree[B]] = x match {
          case Leaf(value) => Eval.now(Leaf(f(value)))
          case Branch(s) => s.unorderedTraverse(go).map(Branch(_))
        }
        go(fa).value
      }
    }

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](a: A): Tree[A] = Leaf(a)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      def go(x: Tree[A]): Eval[Tree[B]] = x match {
        case Leaf(value) => Eval.later(f(value))
        case Branch(s) => s.unorderedTraverse(go).map(Branch(_))
      }
      go(fa).value
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a)) {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => pure(value)
      }
  }

  implicit val treeTraverse: UnorderedTraverse[Tree] =
    new UnorderedTraverse[Tree] {
      override def unorderedTraverse[F[_], A, B](fa: Tree[A])(
        f: A => F[B]
      )(implicit applicative: CommutativeApplicative[F]): F[Tree[B]] =
        fa match {
          case Leaf(value) => applicative.map(f(value))(Leaf(_))
          case Branch(s) =>
            applicative.map(
              s.unorderedTraverse(t => this.unorderedTraverse(t)(f))
            )(Branch(_))
        }

      override def unorderedFoldMap[A, B](
        fa: Tree[A]
      )(f: A => B)(implicit M: CommutativeMonoid[B]): B = {
        def go(t: Tree[A]): B = t match {
          case Leaf(value) => f(value)
          case Branch(s) =>
            s.foldRight(M.empty)((x, acc) => M.combine(go(x), acc))
        }
        go(fa)
      }
    }

  implicit def treeBool[A]: Bool[Tree[A]] = new TreeBool[A]

  // Encoders and decoders are derived from this example:
  // https://circe.github.io/circe/codecs/recursive-adt.html

  implicit def leafDecoder[A](implicit decoder: Decoder[A]): Decoder[Leaf[A]] =
    decoder.map(Leaf(_))
  implicit def branchDecoder[A](implicit
    treeDecoder: Decoder[Tree[A]]
  ): Decoder[Branch[A]] = Decoder.instance { c =>
    c.as[Vector[Tree[A]]].map(vector => Branch(vector.toSet))
  }
  implicit def treeDecoder[A: Decoder]: Decoder[Tree[A]] = Decoder.recursive {
    implicit recurse =>
      List[Decoder[Tree[A]]](
        Decoder[Leaf[A]].widen,
        Decoder[Branch[A]].widen
      ).reduce(_ or _)
  }

  implicit def leafEncoder[A](implicit encoder: Encoder[A]): Encoder[Leaf[A]] =
    Encoder.instance(leaf => leaf.value.asJson(encoder))
  implicit def branchEncoder[A](implicit
    treeEncoder: Encoder[Tree[A]]
  ): Encoder[Branch[A]] =
    Encoder.instance(branch => branch.set.toVector.asJson)
  implicit def treeEncoder[A: Encoder]: Encoder[Tree[A]] = Encoder.recursive {
    recurse =>
      implicit val encoder: Encoder[Tree[A]] = recurse
      Encoder.instance[Tree[A]] {
        case leaf: Leaf[A] => leaf.asJson(leafEncoder)
        case branch: Branch[A] => branch.asJson(branchEncoder)
      }
  }
}

class TreeBool[A] extends Bool[Tree[A]] {
  override def one: Tree[A] = Branch(Set.empty)
  override def zero: Tree[A] = Branch(Set(one))

  override def and(x: Tree[A], y: Tree[A]): Tree[A] =
    Branch(
      Set(
        Branch(
          Set(
            x
          )
        ),
        Branch(
          Set(
            y
          )
        )
      )
    )

  override def or(x: Tree[A], y: Tree[A]): Tree[A] =
    Branch(
      Set(
        Branch(
          Set(
            x,
            y
          )
        )
      )
    )

  override def complement(x: Tree[A]): Tree[A] = Branch(Set(x))
}
