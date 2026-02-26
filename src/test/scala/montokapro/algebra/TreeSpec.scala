package montokapro.algebra

import io.circe.{Decoder, Encoder}
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.funspec.AnyFunSpec

class TreeSpec extends AnyFunSpec {
  import Tree._
  import montokapro.algebra.instances.tree._

  describe("int") {
    def test(string: String, tree: Tree[Int], reduced: Tree[Int], set: Signed[Set[Int]]) = {
      it(string) {
        assert(decode[Tree[Int]](tree.asJson.noSpaces) == Right(tree))
        assert(decode[Tree[Int]](string) == Right(tree))
        assert(tree.reduce() == reduced)
        assert(tree.toSignedSet() == set)
      }
    }

    test(
      "0",
      Leaf(0),
      Leaf(0),
      Signed(false, Set(0))
    )

    test(
      "[0, []]",
      Branch(Set(Leaf(0), Branch(Set.empty))),
      Branch(Set(Branch(Set.empty))),
      Signed(false, Set.empty)
    )

    test(
      "[0, [[]]]",
      Branch(Set(Leaf(0), Branch(Set(Branch(Set.empty))))),
      Branch(Set(Leaf(0))),
      Signed(true, Set(0))
    )

    test(
      "[0, 1, [[2, 3]]]",
      Branch(Set(Leaf(0), Leaf(1), Branch(Set(Branch(Set(Leaf(2), Leaf(3))))))),
      Branch(Set(Leaf(0), Leaf(1), Leaf(2), Leaf(3))),
      Signed(true, Set(0, 1, 2, 3))
    )

    test(
      "[0, [0]]",
      Branch(Set(Leaf(0), Branch(Set(Leaf(0))))),
      Branch(Set(Branch(Set.empty))),
      Signed(false, Set.empty)
    )

    test(
      "[0, [1]]",
      Branch(Set(Leaf(0), Branch(Set(Leaf(1))))),
      Leaf(1),
      Signed(false, Set(1))
    )

    test(
      "[0, [1], [2]]",
      Branch(Set(Leaf(0), Branch(Set(Leaf(1))), Branch(Set(Leaf(2))))),
      Branch(Set(Branch(Set.empty))),
      Signed(false, Set.empty)
    )

    test(
      "[0, [1, 2], [[3]]]",
      Branch(Set(Leaf(0), Branch(Set(Leaf(1), Leaf(2))), Branch(Set(Branch(Set(Leaf(3))))))),
      Branch(Set(Branch(Set(Leaf(1), Leaf(2))))),
      Signed(false, Set(1, 2))
    )

    test(
      "[0, 1, [1, 2, [2, 3]]]",
      Branch(Set(Leaf(0), Leaf(1), Branch(Set(Leaf(1), Leaf(2), Branch(Set(Leaf(2), Leaf(3))))))),
      Branch(Set(Leaf(0), Leaf(1), Leaf(3))),
      Signed(true, Set(0, 1, 3))
    )
  }

  describe("set") {
    def test(
      string: String,
      setTree: Tree[Set[Int]],
      setReduced: Tree[Set[Int]],
      tree: Tree[Int],
      reduced: Tree[Int],
      set: Signed[Set[Int]]
    ) = {
      it(string) {
        assert(decode[Tree[Set[Int]]](setTree.asJson.noSpaces) == Right(setTree))
        assert(decode[Tree[Set[Int]]](string) == Right(setTree))
        assert(setTree.flatReduce() == setReduced)

        assert(decode[Tree[Int]](tree.asJson.noSpaces) == Right(tree))
        assert(decode[Tree[Int]](string) == Right(tree))
        assert(tree.reduce() == reduced)

        assert(setTree.toFlatTree() == tree)
        assert(setTree.toFlatSignedSet() == set)
        assert(tree.toSignedSet() == set)
      }
    }

    test(
      "[]",
      Leaf(Set.empty),
      Leaf(Set.empty),
      Branch(Set.empty),
      Branch(Set.empty),
      Signed(true, Set.empty)
    )

    test(
      "[0]",
      Leaf(Set(0)),
      Leaf(Set(0)),
      Branch(Set(Leaf(0))),
      Branch(Set(Leaf(0))),
      Signed(true, Set(0))
    )

    test(
      "[0, 1]",
      Leaf(Set(0, 1)),
      Leaf(Set(0, 1)),
      Branch(Set(Leaf(0), Leaf(1))),
      Branch(Set(Leaf(0), Leaf(1))),
      Signed(true, Set(0, 1))
    )

    test(
      "[[]]",
      Branch(Set(Leaf(Set.empty))),
      Branch(Set(Leaf(Set.empty))),
      Branch(Set(Branch(Set.empty))),
      Branch(Set(Branch(Set.empty))),
      Signed(false, Set.empty)
    )

    test(
      "[[[0, 1, 2], [1, 2, 3]], [[2, 3, 4]]]",
      Branch(Set(
        Branch(Set(
          Leaf(Set(0, 1, 2)),
          Leaf(Set(1, 2, 3)),
        )),
        Branch(Set(
          Leaf(Set(2, 3, 4)),
        ))
      )),
      Leaf(Set(1, 2, 3, 4)),
      Branch(Set(
        Branch(Set(
          Branch(Set(Leaf(0), Leaf(1), Leaf(2))),
          Branch(Set(Leaf(1), Leaf(2), Leaf(3)))
        )),
        Branch(Set(
          Branch(Set(Leaf(2), Leaf(3), Leaf(4)))
        ))
      )),
      Branch(Set(Leaf(1), Leaf(2), Leaf(3), Leaf(4))),
      Signed(true, Set(1, 2, 3, 4))
    )
  }
}
