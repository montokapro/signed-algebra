package montokapro.algebra

sealed trait Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](set: Set[Tree[A]]) extends Tree[A]

object Tree {
  def fromSignedSet[A](set: Signed[Set[A]]): Tree[A] = {
    if (set.negative) {
      Branch(set.value.map(Leaf.apply))
    } else if (set.value.size == 1) {
      Leaf(set.value.head)
    } else {
      Branch(Set(Branch(set.value.map(Leaf.apply))))
    }
  }

  def fromFlatSignedSet[A](set: Signed[Set[A]]): Tree[Set[A]] = {
    if (set.negative) {
      Leaf(set.value)
    } else {
      Branch(Set(Leaf(set.value)))
    }
  }
}
