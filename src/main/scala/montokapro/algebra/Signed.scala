package montokapro.algebra

case class Signed[+A](negative: Boolean, value: A)
