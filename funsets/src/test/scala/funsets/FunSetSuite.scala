package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  ignore("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  ignore("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val u : Int = 10
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(3)
    def ten = (x: Int) => x >= 0 && x < 10
    def odd = (x: Int) => x % 2 != 0
    def even = (x: Int) => x % 2 == 0
    def rnd = (x: Int) => x == 3 || x == 17 || x == 21 
    def empty = (x : Int) => x > 1000   
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val u1 = union(odd, even)
      val u2 = union(ten, rnd) 
      println("Union..")
      printSet(u2)
      assert(contains(u1, 1), "Union 1")
      assert(contains(u1, 2), "Union 2")
      assert(contains(u1, -3), "Union 3")
      assert(contains(u2, 17), "Union 4")
      assert(contains(u2, 5), "Union 4")
      assert(!contains(u2, -3), "Union 3")
    }
  }
  
  test("intersect contains common elements") {
    new TestSets {
      val i1 = intersect(s1, s2)
      println("Intersect..")
      printSet(i1)
      assert(!contains(i1, 1), "intersect 1")
      assert(!contains(i1, 2), "intersect 2")
      val i2 = intersect(s3,s4)
      printSet(i2)
      assert(contains(i2, 3), "intersect 3")
      val i3 = intersect(ten,rnd)
      printSet(i3)
      assert(contains(i3, 3), "intersect 4") 
       assert(!contains(i3, 17), "intersect 5") 
    }
      
  }
  
  test("difference contains elements of set 1 not in set 2") {
    new TestSets {
      val d1 = diff(odd, even)
      val d2 = diff(ten, rnd)
      val d3 = diff(rnd, ten)
      println("Differences..")
      printSet(d2)
      printSet(d3)
      assert(contains(d1, 3), "difference 1")
      assert(!contains(d1, 2), "difference 2") 
      assert(contains(d2, 6), "difference 3")
      assert(!contains(d2, 3), "difference 4")
      assert(contains(d3, 17), "difference 5")
      assert(!contains(d3, 6), "difference 6")
    }
  }
  
  test("filter contains elements that hold the predicate") {
    new TestSets {
      val f1 = filter(ten, (x : Int) => x % 2 == 0 )
      val f2 = filter(ten, (x : Int) => x % 2 != 0 )
      println("Filter sets..")
      printSet(f1)
      printSet(f2)
      assert(contains(f1, 8), "filter 1")
      assert(!contains(f1, 3), "filter 2") 
      assert(contains(f2, 9), "filter 3")
      assert(!contains(f2, 4), "filter 4")
      assert(!contains(f2, 15), "filter 5")
    }
  }
  
  test("forall checks if all elements hold the predicate") {
    new TestSets {
      
      assert(!forall(ten, (x : Int) => x % 2 == 0 ), "forall 1")
      assert(forall(odd, (x : Int) => x % 2 != 0 ), "forall 2")
      assert(forall(rnd, (x : Int) => x < 30 ), "forall 3")
      assert(forall(rnd, (x : Int) => x < 30 ), "forall 3")
           
    }
  }
  
  test("empty sets") {
    new TestSets {
      assert(forall(empty, (x : Int) => x % 2 == 0 ), "empty")
      assert(!exists(empty, (x : Int) => x < 10 ), "exists 3")
    }
  }
  
  test("exists checks if atleast one element hold the predicate") {
    new TestSets {
      
      assert(exists(ten, (x : Int) => x % 2 == 0 ), "exists 1")
      assert(!exists(odd, (x : Int) => x % 2 == 0 ), "exists 2")
      assert(exists(rnd, (x : Int) => x < 10 ), "exists 3")
      
    }
  }
  
  test("map changes the set elements to another element by using a given function") {
    new TestSets {
      val m1 = map(ten, (x : Int) => x *x )
      val m2 = map(ten, (x : Int) => -x  )
      assert(contains(m1,4), "map 1")
      assert(!contains(m1,100), "map 2")
      assert(contains(m2, -3), "map 3")
      assert(contains(m2, -3), "map 4")
      
    }
  }
}
