package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  /*lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)*/

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(genHeap, Gen.const(empty))
  } yield insert(x, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

/*
* 1/ If you insert any two elements into an empty heap, finding
*   the minimum of the resulting heap should get the smallest of the two elements back.
*/
  property("hint1")=forAll{(a1:A,a2:A)=>
    val h=insert(a2,insert(a1,empty))
    if(a1>a2)findMin(h)==a2 else findMin(h)==a1}

/*
*2/ If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
 */
  property("hint2")=forAll{a:A=>deleteMin(insert(a,empty))==empty}
/*
* 3/ Given any heap, you should get a sorted sequence of elements
* when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
*/
  property("hint3") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(h)
  }
/*
*4/ Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
 */
  property("hint4")=forAll{
    (h1:H,h2:H)=>
      val m=meld(h1,h2)
      if(findMin(h1)<findMin(h2)) findMin(m)==findMin(h1)
      else findMin(m)==findMin(h2)
  }
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  /* https://class.coursera.org/reactive-001/forum/thread?thread_id=97#post-371 */
  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

}
