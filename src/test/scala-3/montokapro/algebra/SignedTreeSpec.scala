package montokapro.algebra

import cats.Monad
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
    it("[[1 2 3] [2 3 4]] [[3 4 5]]") {
      assert(t(1, t(2, 3), t(t(4))).reduce() == s(true, 2, 3))
    }
  }

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

  it("disjunctive normal form") {
    import montokapro.algebra.instances.signedTree._

    val string: String = "[[[0, 1, 2], [1, 2, 3]], [[2, 3, 4]]]"

    val tree: SignedTree[Set[Int]] =
      SignedTree[Set[Int]](
        Set(),
        Set(
          SignedTree(
            Set(Set(0, 1, 2), Set(1, 2, 3)),
            Set()
          ),
          SignedTree(
            Set(Set(2, 3, 4)),
            Set()
          )
        )
      )

    val set = Signed(false, Set(1, 2, 3, 4))

    def go(a: Set[Int]): SignedTree[Int] = SignedTree(a, Set())

    assert(tree.inverseFlatMap(go).reduce() == set)
    assert(decode[SignedTree[Int]](string).map(_.reduce()) == Right(set))
    assert(decode[SignedTree[Int]](string).map(_.reduce()) == Right(tree.inverseFlatMap(go).reduce()))
  }

  it("nested flatMap") {
    import montokapro.algebra.instances.signedTree._

    val string: String = "[0, [1, [2]]]"

    val tree: SignedTree[Set[Int]] =
      SignedTree[Set[Int]](
        Set(Set(0)),
        Set(
          SignedTree(
            Set(Set(1)),
            Set(
              SignedTree(
                Set(Set(2)),
                Set()
              )
            )
          )
        )
      )

    val set = Signed(false, Set(0, 2))

    def go(a: Set[Int]): SignedTree[Int] = SignedTree(a, Set())

    assert(tree.flatMap(go).reduce() == set)
    assert(decode[SignedTree[Int]](string).map(_.reduce()) == Right(set))
    assert(decode[SignedTree[Int]](string).map(_.reduce()) == Right(tree.flatMap(go).reduce()))
  }

  it("nested flatMap 2") {
    import montokapro.algebra.instances.signedTree._

    val string: String = "[0, 1, [1, 2, [2, 3]]]"

    val tree: SignedTree[Set[Int]] =
      SignedTree[Set[Int]](
        Set(Set(0, 1)),
        Set(
          SignedTree(
            Set(Set(1, 2)),
            Set(
              SignedTree(
                Set(Set(2, 3)),
                Set()
              )
            )
          )
        )
      )

    val set = Signed(false, Set(0, 1, 3))

    def go(a: Set[Int]): SignedTree[Int] = SignedTree(a, Set())

    assert(tree.flatMap(go).reduce() == set)
    assert(decode[SignedTree[Int]](string).map(_.reduce()) == Right(set))
    assert(decode[SignedTree[Int]](string).map(_.reduce()) == Right(tree.flatMap(go).reduce()))
  }

  it("nested inverseFlatMap") {
    import montokapro.algebra.instances.signedTree._

    val string: String = "[[0, 1, 2], [[1, 2, 3], [2, 3, 4]]]"

    val tree: SignedTree[Set[Int]] = SignedTree[Set[Int]](
      Set(Set(0, 1, 2)),
      Set(
        SignedTree(
          Set(
            Set(1, 2, 3),
            Set(2, 3, 4)
          ),
          Set()
        )
      )
    )

    val set = Signed(true, Set(0, 1))

    def go(a: Set[Int]): SignedTree[Int] = SignedTree(a, Set())

    assert(tree.inverseFlatMap(go).reduce() == set)
    assert(decode[SignedTree[Int]](string).map(_.reduce()) == Right(set))
    assert(decode[SignedTree[Int]](string).map(_.reduce()) == Right(tree.inverseFlatMap(go).reduce()))
  }
}
