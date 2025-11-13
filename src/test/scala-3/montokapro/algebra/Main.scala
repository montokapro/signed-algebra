package montokapro.algebra

import cats.implicits._
import io.circe.{ Encoder, Json }
import io.circe.parser._
import io.circe.syntax._

import instances.all._

implicit val signedIntEncoder: Encoder[Signed[Set[Int]]] = Encoder.instance[Signed[Set[Int]]] { a =>
  Json.obj(
    ("negative", a.negative.asJson),
    ("value", a.value.asJson)
  )
}

// Example
//
// > sbt
// > Test/run "[[0, 1, 2], [[1, 2, 3], [2, 3, 4]]]"
@main def reduce(s: String) = println(
  decode[SignedTree[Int]](s) match {
    case Left(e) => e
    case Right(tree) => tree.reduce().asJson
  }
)
