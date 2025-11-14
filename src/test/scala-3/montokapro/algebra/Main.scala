package montokapro.algebra

import cats.implicits._
import io.circe.{ Encoder, Json }
import io.circe.parser._
import io.circe.syntax._

import instances.all._

// The following example
//
// > sbt
// > Test/run "[[0, 1, 2], [[1, 2, 3], [2, 3, 4]]]"
//
// will return
//
// [
//   [
//     0,
//     1
//   ]
// ]
//
// We can reduce this ourselves to check correctness
//
// [[0, 1, 2], [[1, 2, 3], [2, 3, 4]]]
// by intersection
// [[0, 1, 2], [[2, 3]]]
// by double negation elimination
// [[0, 1, 2], 2, 3]
// by subtraction
// [[0, 1]]
@main def reduce(s: String) = println(
  decode[SignedTree[Int]](s) match {
    case Left(e) => e
    case Right(tree) => SignedTree.fromSignedSet(tree.reduce()).asJson
  }
)
