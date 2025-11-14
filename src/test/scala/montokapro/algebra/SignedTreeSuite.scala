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

class SignedTreeSuite
    extends AnyFunSuite
    with FunSuiteDiscipline
    with Configuration {
  implicit def arbSignedTree[A: Arbitrary]: Arbitrary[SignedTree[A]] = {
    val maxDepth = 3

    def genTree(pSize: Int): Gen[SignedTree[A]] = {
      val nSize = pSize - 1
      for {
        p <- Gen.choose(0, pSize)
        n <- Gen.choose(0, nSize)
        positives <- Gen.listOfN(p, Arbitrary.arbitrary[A])
        negatives <- Gen.listOfN(n, genTree(nSize))
      } yield SignedTree(positives.toSet, negatives.toSet)
    }

    Arbitrary(genTree(maxDepth))
  }

  implicit def signedSetEq[A]: Eq[Signed[Set[A]]] =
    Eq.fromUniversalEquals[Signed[Set[A]]]
  implicit def signedTreeEq[A]: Eq[SignedTree[A]] = Eq.by(_.toSignedSet())

  checkAll("SignedTree.FunctorLaws", FunctorTests[SignedTree].functor[Int, Int, String])
  checkAll("SignedTree.MonadLaws", MonadTests[SignedTree].stackUnsafeMonad[Int, Int, String])
  checkAll("SignedTree.UnorderedTraverseLaws", UnorderedTraverseTests[SignedTree].unorderedTraverse[Int, Double, String, Option, Option])
  checkAll("SignedTreeBoolean.LogicLaws", LogicLaws[SignedTree[Boolean]].bool)
  checkAll("SignedTreeInt.LogicLaws", LogicLaws[SignedTree[Int]].bool)
  checkAll("SignedTreeInt.CodecLaws", CodecTests[SignedTree[Int]].codec)
}
