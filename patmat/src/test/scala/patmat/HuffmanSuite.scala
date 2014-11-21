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
    val strList = List('h', 'h', 'l', 'l', 'l', 'a', 'b', 'b', 'b', 'b')
    val strList2 = List('h', 'h', 'l', 'l', 'l', 'l', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'b')
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

  test("times and makeOrdered test of a tree") {
	 new TestTrees {
	     val t = times(strList)
	     assert(t.size === 4)
		 val ordered = makeOrderedLeafList(t)
	//	 ordered.foreach(println)
		 assert(ordered.head.char === 'a') 
		 assert(ordered.head.weight === 1)
		 assert(ordered.tail.head.char === 'h') 
		 assert(ordered.tail.head.weight === 2)
	 }
}
  
  test("singleton test") {
    new TestTrees {
      val single = List(Leaf('a',2))
      val two = List(Leaf('a',2),Leaf('b',4))
      assert(singleton(single)===true)
      assert(singleton(two) === false)
      assert(singleton(List()) === false)
    }
  }
  
  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of hello world list") {
    new TestTrees {
    	val ordered = combine(makeOrderedLeafList(times(strList2)))
    	//ordered.foreach(println)
    	assert(ordered === List(Leaf('l',4),Fork(Leaf('h',2), Leaf('a',3), List('h','a'),5), Leaf('b',6)))
    }
  }

  test("combine of createCodeTree") {
    new TestTrees {
    	val tree = createCodeTree(strList2)
    	//List(tree).foreach(println)
    	assert(tree === Fork(Leaf('b',6),
    			                        Fork(Leaf('l',4),
    			                                        Fork(Leaf('h',2),Leaf('a',3),List('h', 'a'),5),
    			                        List('l', 'h', 'a'),9),
    			             List('b', 'l', 'h', 'a'),15))
    }
  }
  
  test("decode test") {
    new TestTrees {
    	val tree = createCodeTree(strList2)
    	val dec = decode(tree,List(1,1,0,1,1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0))
    	assert(dec === strList2.toList)
    	
    	val dec2 = decode(createCodeTree(List('a','a','b')), List(1,1,0))
    	//println()
    	//dec2.foreach(print)
    	assert(dec2 === List('a','a','b'))
    	//println()
    	decodedSecret.foreach(print)
    	//println()
     }
  }
  
    
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      
      val str1 = "For i am a good player in the universe of sport of nevada"
      
      val ct = createCodeTree(str1.toList)
      val enc = encode(ct)(str1.toList)
      
      assert(decode(ct,enc) === str1.toList)
      
      val str2 = "For a great player mages"
      val enc2 = encode(ct)(str2.toList)
      assert(decode(ct,enc2) === str2.toList)
      
     /* val str3 = "F"
      val ct2 = createCodeTree(str3.toList)
      val enc3 = encode(ct)(str3.toList)
      assert(decode(ct2,enc3) === str3.toList)
      
      val str4 = "FFFFF"
      val enc4 = encode(ct2)(str4.toList)
      println(enc4)
      assert(decode(ct2,enc4) === str4.toList)
      */
    }
  }
  
  test("convert test") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      
      val str1 = "For i am a good player in the universe of sport of nevada"
      
      val ct = createCodeTree(str1.toList)
      println(ct)
      val conv = convert(ct)
      println(conv)
      
      val tree = createCodeTree(strList2)
      val conv2 = convert(tree)
      println(tree)
      println(conv2)
      //assert(decode(ct,enc) === str1.toList)
      
     
    }
  }
  test("quickencode test") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
      
      val str1 = "For i am a good player in the universe of sport of nevada"
      
      val ct = createCodeTree(str1.toList)
      val enc = quickEncode(ct)(str1.toList)
      
      assert(decode(ct,enc) === str1.toList)
      
      val str2 = "For a great player mages"
      val enc2 = quickEncode(ct)(str2.toList)
      assert(decode(ct,enc2) === str2.toList)
     
    }
  }
}
