package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[A]
      v <- oneOf(const(empty), genHeap)
    } yield this.insert(k, v)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == Math.min(a, b)
  }

  // If you insert an element into an empty heap, then delete the minimum,
  // the resulting heap should be empty.
  property("empty") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding
  // and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("ord1") = forAll { h: H =>
    def checkOrd(xh: H, min: A): Boolean = {
      if (isEmpty(xh)) true
      else if (findMin(xh) >= min) checkOrd(deleteMin(xh), findMin(xh))
      else false
    }
    if (isEmpty(h)) true
    else checkOrd(deleteMin(h), findMin(h))
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("min3") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val v1 = findMin(h1)
      val v2 = findMin(h2)
      val min = Math.min(v1, v2)
      v1 == v2 && findMin(meld(deleteMin(h1), insert(min, h2))) == min
    }
  }
}
