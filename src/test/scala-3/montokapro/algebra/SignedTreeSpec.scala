package montokapro.algebra

import io.circe.{Decoder, Encoder}
import io.circe.parser._
import io.circe.syntax._
import montokapro.algebra._
import org.scalatest.funspec.AnyFunSpec

class SignedTreeSpec extends AnyFunSpec {
  def s(negative: Boolean, xs: Int*) = {
    Signed[Set[Int]](negative, Set.from(xs))
  }

  def t(xs: (Int | SignedTree[Int])*) = {
    val ps = Set.newBuilder[Int]
    val ns = Set.newBuilder[SignedTree[Int]]
    xs.foreach {
      case p: Int => ps += p
      case n: SignedTree[Int] => ns += n
    }
    SignedTree(ps.result(), ns.result())
  }

  describe("reduce") {
    it("") {
      assert(t().reduce() == s(false))
    }
    it("[]") {
      assert(t(t()).reduce() == s(true))
    }
    it("1") {
      assert(t(1).reduce() == s(false, 1))
    }
    it("1 []") {
      assert(t(1, t()).reduce() == s(true))
    }
    it("1 [[]]") {
      assert(t(1, t(t())).reduce() == s(false, 1))
    }
    it("1 2 [[3 4]]") {
      assert(t(1, 2, t(t(3, 4))).reduce() == s(false, 1, 2, 3, 4))
    }
    it("1 [1]") {
      assert(t(1, t(1)).reduce() == s(true))
    }
    it("1 [2]") {
      assert(t(1, t(2)).reduce() == s(true, 2))
    }
    it("1 [2] [3]") {
      assert(t(1, t(2), t(3)).reduce() == s(true))

    }
    it("1 [2 3] [[4]]") {
      assert(t(1, t(2, 3), t(t(4))).reduce() == s(true, 2, 3))
    }
  }

  describe("encode") {
    it("") {
      assert(t().reduce() == s(false))
    }
    it("[]") {
      assert(t(t()).reduce() == s(true))
    }
    it("1") {
      assert(t(1).reduce() == s(false, 1))
    }
    it("1 []") {
      assert(t(1, t()).reduce() == s(true))
    }
    it("1 [[]]") {
      assert(t(1, t(t())).reduce() == s(false, 1))
    }
    it("1 2 [[3 4]]") {
      assert(t(1, 2, t(t(3, 4))).reduce() == s(false, 1, 2, 3, 4))
    }
    it("1 [1]") {
      assert(t(1, t(1)).reduce() == s(true))
    }
    it("1 [2]") {
      assert(t(1, t(2)).reduce() == s(true, 2))
    }
    it("1 [2] [3]") {
      assert(t(1, t(2), t(3)).reduce() == s(true))

    }
    it("1 [2 3] [[4]]") {
      assert(t(1, t(2, 3), t(t(4))).reduce() == s(true, 2, 3))
    }
  }

  // Consider
  // https://circe.github.io/circe/codecs/testing.html
  // https://github.com/circe/circe/blob/series/0.14.x/modules/testing/shared/src/main/scala/io/circe/testing/ParserTests.scala
  describe("circe") {
    import montokapro.algebra.instances.signedTree._

    val complex: String = "[3, [2, []], [[1], [1]], 0]"
    it(complex) {
      val tree: SignedTree[Int] = SignedTree(
        Set(0, 3),
        Set(
          SignedTree(
            Set(2),
            Set(
              SignedTree(
                Set(),
                Set(),
              )
            )
          ),
          SignedTree(
            Set(),
            Set(
              SignedTree(
                Set(1),
                Set()
              )
            )
          )
        )
      )
      assert(decode[SignedTree[Int]](complex) == Right(tree))
      assert(tree.asJson.as[SignedTree[Int]] == Right(tree))
    }

    it("") {
      assert(decode[SignedTree[Int]]("").isLeft)
    }

    it("0") {
      assert(decode[SignedTree[Int]]("0").isLeft)
    }

    it("[0]") {
      assert(decode[SignedTree[String]]("[0]").isLeft)
    }
  }
}
