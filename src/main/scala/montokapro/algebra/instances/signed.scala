package montokapro.algebra
package instances

import montokapro.algebra.lattice.Signed
import algebra.lattice.{GenBool, Bool}
import cats.kernel.Eq
import cats.Functor

package object signed extends SignedInstances

trait SignedInstances {
  implicit val functor: Functor[Signed] = new Functor[Signed] {
    override def map[A, B](fa: Signed[A])(f: A => B) =
      Signed(fa.negative, f(fa.value))
  }

  implicit def eq[A: Eq]: Eq[Signed[A]] =
    new SignedEq[A]

  implicit def bool[A: GenBool]: Bool[Signed[A]] = new SignedBool[A]
}

class SignedEq[A](implicit A: Eq[A]) extends Eq[Signed[A]] {
  def eqv(x: Signed[A], y: Signed[A]): Boolean =
    Eq[Boolean].eqv(x.negative, y.negative) && A.eqv(x.value, y.value)
}

class SignedBool[A](implicit ev: GenBool[A]) extends Bool[Signed[A]] {
  override def zero: Signed[A] = Signed(false, ev.zero)
  override def one: Signed[A] = Signed(true, ev.zero)

  def isZero(x: Signed[A])(implicit eq: Eq[A]): Boolean = x.negative match {
    case false =>
      ev.isZero(x.value)
    case true =>
      false // can produce false negative
  }

  def isOne(x: Signed[A])(implicit eq: Eq[A]): Boolean = x.negative match {
    case false =>
      false // can produce false negative
    case true =>
      ev.isZero(x.value)
  }

  override def or(x: Signed[A], y: Signed[A]): Signed[A] =
    (x.negative, y.negative) match {
      case (false, false) =>
        Signed(false, ev.or(x.value, y.value))
      case (false, true) =>
        Signed(true, ev.without(y.value, x.value))
      case (true, false) =>
        Signed(true, ev.without(x.value, y.value))
      case (true, true) =>
        Signed(true, ev.and(x.value, y.value))
    }

  override def and(x: Signed[A], y: Signed[A]): Signed[A] =
    (x.negative, y.negative) match {
      case (false, false) =>
        Signed(false, ev.and(x.value, y.value))
      case (false, true) =>
        Signed(false, ev.without(x.value, y.value))
      case (true, false) =>
        Signed(false, ev.without(y.value, x.value))
      case (true, true) =>
        Signed(true, ev.or(x.value, y.value))
    }

  override def complement(x: Signed[A]): Signed[A] = Signed(!x.negative, x.value)
}
