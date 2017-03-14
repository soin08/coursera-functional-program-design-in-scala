package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      e <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(e, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, finding the
  // minimum of the resulting heap should get the smallest of the two
  // elements back.
  property("findMin: heap of two elements") = forAll { (e1: A, e2: A) =>
    val h = insert(e2, insert(e1, empty))
    val smaller = if (e1 < e2) e1 else e2
    findMin(h) == smaller
  }

  // Finding a minimum of the melding of any two heaps should return
  // a minimum of one or the other.
  property("findMin: two heaps") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val h = meld(h1, h2)
      findMin(h) == findMin(h1) || findMin(h) == findMin(h2)
    }
  }

  // If you insert an element into an empty heap, then delete the
  // minimum, the resulting heap should be empty.
  property("deleteMin: heap of one element") = forAll { (e: A) =>
    val h = insert(e, empty)
    isEmpty(deleteMin(h))
  }

  // Given any heap, you should get a sorted sequence of elements
  // when continually finding and deleting min.
  property("deleteMin: continually") = forAll { (h: H) =>
    def heapToList(h: H): List[A] = {
      if (isEmpty(h)) List()
      else findMin(h) :: heapToList(deleteMin(h))
    }
    val l = heapToList(h)
    l == l.sorted
  }

}
