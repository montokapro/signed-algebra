package montokapro.algebra

import montokapro.algebra.instances.all._

import algebra.instances.all._
import algebra.laws.LogicLaws
import cats.Eq
import cats.implicits._
import cats.instances.all._
import cats.laws.discipline.{FunctorTests, MonadTests, UnorderedTraverseTests}
import io.circe.testing.instances._
import io.circe.testing.CodecTests
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class TreeSuite
    extends AnyFunSuite
    with FunSuiteDiscipline
    with Configuration {
  implicit def arbTree[A: Arbitrary]: Arbitrary[Tree[A]] = {
    val maxDepth = 3

    def genTree(size: Int): Gen[Tree[A]] =
      if (size <= 0) {
        Arbitrary.arbitrary[A].map(Leaf(_))
      } else {
        Gen.choose(0, size)
          .flatMap(n => Gen.listOfN(n, genTree(size - 1))
          .map(trees => Branch(trees.toSet)))
      }

    Arbitrary(genTree(maxDepth))
  }

  implicit def signedSetEq[A]: Eq[Signed[Set[A]]] =
    Eq.fromUniversalEquals[Signed[Set[A]]]
  implicit def treeEq[A]: Eq[Tree[A]] = Eq.by(_.toSignedSet())

  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
  checkAll("Tree.MonadLaws", MonadTests[Tree].stackUnsafeMonad[Int, Int, String])
  checkAll("Tree.UnorderedTraverseLaws", UnorderedTraverseTests[Tree].unorderedTraverse[Int, Double, String, Option, Option])
  checkAll("TreeBoolean.LogicLaws", LogicLaws[Tree[Boolean]].bool)
  checkAll("TreeInt.LogicLaws", LogicLaws[Tree[Int]].bool)
  checkAll("TreeInt.CodecLaws", CodecTests[Tree[Int]].unserializableCodec)
}
