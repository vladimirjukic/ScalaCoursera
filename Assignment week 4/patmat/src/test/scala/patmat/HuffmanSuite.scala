package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert(times(List('a','b','d','a', 'b','f')) === List(('b', 2), ('d', 1), ('a', 2), ('f', 1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until test") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) == List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  test("test decode t1") {
    new TestTrees {
      assert(decode(t1, List(1, 0)) === List('b', 'a'))
    }
  }

  test("test decode t2") {
    new TestTrees {
      assert(decode(t2, List(1, 0, 1, 0, 1)) === List('d', 'b', 'b'))
    }
  }

  test("secret message") {
    println(decodedSecret.toString())
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits exists test") {
    val l1 = List(('a', List(0, 1)), ('b', List(1, 0)), ('f', List(1, 1, 0)))
      assert(codeBits(l1)('b') === List(1, 0))
  }

  test("codeBits doesn't exist test") {
    val l1 = List(('a', List(0, 1)), ('b', List(1, 0)), ('f', List(1, 1, 0)))
    assert(codeBits(l1)('c') === List())
  }

  test("convert") {
    new TestTrees {
      assert(convert(t2) ===  List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }

  test("mergeCodeTables test") {
    val l1 = List(('a', List(0, 1)), ('b', List(1, 0)))
    val l2 = List(('c', List(0, 1, 1)), ('d', List(1, 0, 1)))
    assert(mergeCodeTables(l1, l2) === List(('a', List(0, 1)), ('b', List(1, 0)), ('c', List(0, 1, 1)), ('d', List(1, 0, 1))))
  }

  test("quickEncode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
