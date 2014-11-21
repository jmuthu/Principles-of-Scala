package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }
  
  test("union: all tweets") {
    new TestSets {
      assert(size(TweetReader.allTweets) === 695 )
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
     // trends.foreach(println)
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
  
  
  
  test("trending") {
    new TestSets {
       val s2 = set5.incl(new Tweet("a", "a body android", 20))
       val s3 = s2.incl(new Tweet("b", "b body ios", 20))
       val s4 = s3.incl(new Tweet("c", "c body nexus ", 7))
       val s5 = s4.incl(new Tweet("d", "d body ipad", 7))
       
       //val s = "android txt"
      // println(GoogleVsApple.google.exists(elem => s.contains(elem)))
       val g: TweetSet = s5.filter(t => GoogleVsApple.google.exists(elem => t.text.contains(elem)))
       val a: TweetSet = s5.filter(t => GoogleVsApple.apple.exists(elem => t.text.contains(elem)))
        //g.foreach(println)
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
      val t: TweetList = g.union(a).descendingByRetweet
      t.foreach(println)
      assert(!t.isEmpty)
      
    }
  }
}
