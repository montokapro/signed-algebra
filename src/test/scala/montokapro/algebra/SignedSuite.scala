package montokapro.algebra

import montokapro.algebra.lattice._
import montokapro.algebra.instances.signed._
import algebra.instances.all._
import algebra.laws.LogicLaws
import cats.implicits._
import cats.laws.discipline.FunctorTests
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

// signedEq implicits are needed for Scala 2 compilation
import montokapro.algebra.instances.SignedEq
import cats.Eq

class SignedSuite
    extends AnyFunSuite
    with FunSuiteDiscipline
    with Configuration {
  implicit def arbSigned[A: Arbitrary]: Arbitrary[Signed[A]] =
    Arbitrary(for {
      b <- Gen.oneOf(false, true)
      a <- Arbitrary.arbitrary[A]
    } yield Signed(b, a))

  // signedEq implicits are needed for Scala 2 compilation
  implicit val signedEqInt: Eq[Signed[Int]] = new SignedEq[Int]
  implicit val signedEqString: Eq[Signed[String]] = new SignedEq[String]
  implicit val signedEqBoolean: Eq[Signed[Boolean]] = new SignedEq[Boolean]
  implicit val signedEqSetByte: Eq[Signed[Set[Byte]]] = new SignedEq[Set[Byte]]

  checkAll("Signed.FunctorLaws", FunctorTests[Signed].functor[Int, Int, String])
  checkAll("SignedBoolean.LogicLaws", LogicLaws[Signed[Boolean]].bool)
  checkAll("SignedSet.LogicLaws", LogicLaws[Signed[Set[Byte]]].bool)
}
