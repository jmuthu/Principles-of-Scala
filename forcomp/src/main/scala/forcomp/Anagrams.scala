package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
    * `Occurrences` is a `List` of pairs of characters and positive integers saying
    *  how often the character appears.
    *  This list is sorted alphabetically w.r.t. to the character in each pair.
    *  All characters in the occurrence list are lowercase.
    *
    *  Any list of pairs of lowercase characters and their frequency which is not sorted
    *  is **not** an occurrence list.
    *
    *  Note: If the frequency of some character is zero, then that character should not be
    *  in the list.
    */
  type Occurrences = List[(Char, Int)]

  /**
    * The dictionary is simply a sequence of words.
    *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary : List[Word] = loadDictionary

  /**
    * Converts the word into its character occurence list.
    *
    *  Note: the uppercase and lowercase version of the character are treated as the
    *  same character, and are represented as a lowercase character in the occurrence list.
    */
  def wordOccurrences(w : Word) : Occurrences = {
    val wordClean = w.toLowerCase().filter(l ⇒ l.isLetter)
    val charCntMap = wordClean groupBy (l ⇒ l) map { case (ch, str) ⇒ (ch -> str.length) }
    charCntMap.toList.sorted
    /* def wordOccur(word:Word): Map[Char,Int] = {      
      if (word.isEmpty()) Map() withDefaultValue 0
      else {
        val m = wordOccur(word drop 1)
        m + (word.charAt(0)-> (m(word.charAt(0))+1))
      }
    }
    wordOccur(wordClean).toList
    */
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s : Sentence) : Occurrences = wordOccurrences(s mkString "")

  /**
    * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    *  the words that have that occurrence count.
    *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    *  For example, the word "eat" has the following character occurrence list:
    *
    *     `List(('a', 1), ('e', 1), ('t', 1))`
    *
    *  Incidentally, so do the words "ate" and "tea".
    *
    *  This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    *
    */
  lazy val dictionaryByOccurrences : Map[Occurrences, List[Word]] = dictionary groupBy (w ⇒ wordOccurrences(w)) withDefaultValue List()

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word : Word) : List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /**
    * Returns the list of all subsets of the occurrence list.
    *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    *  is a subset of `List(('k', 1), ('o', 1))`.
    *  It also include the empty subset `List()`.
    *
    *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    *    List(
    *      List(),
    *      List(('a', 1)),
    *      List(('a', 2)),
    *      List(('b', 1)),
    *      List(('a', 1), ('b', 1)),
    *      List(('a', 2), ('b', 1)),
    *      List(('b', 2)),
    *      List(('a', 1), ('b', 2)),
    *      List(('a', 2), ('b', 2))
    *    )
    *
    *  Note that the order of the occurrence list subsets does not matter -- the subsets
    *  in the example above could have been displayed in some other order.
    */
  def combinations(occurrences : Occurrences) : List[Occurrences] = occurrences match {
    case Nil ⇒ List(List())
    case head :: tail ⇒ {
      val subsetComb = combinations(tail)
      val headComb = for {
        x ← 1 to head._2;
        y ← subsetComb
      } yield (head._1, x) :: y
      headComb.toList ::: subsetComb
    }

  }
  /**
    *
    * def combWithSeqNo(occur : Occurrences, n : Int) : List[Occurrences] = {
    * if (n == 0)
    * List(List())
    * else
    * for {
    * x ← (0 to (occur.length - n)).toList;
    * z ← 1 to occur(x)._2;
    * y ← combWithSeqNo(occur drop (x + 1), n - 1)
    * } yield (occur(x)._1, z) :: y
    * }
    * for { x ← (0 to occurrences.length).toList; m ← combWithSeqNo(occurrences, x) } yield m
    *
    *
    */

  /**
    * Subtracts occurrence list `y` from occurrence list `x`.
    *
    *  The precondition is that the occurrence list `y` is a subset of
    *  the occurrence list `x` -- any character appearing in `y` must
    *  appear in `x`, and its frequency in `y` must be smaller or equal
    *  than its frequency in `x`.
    *
    *  Note: the resulting value is an occurrence - meaning it is sorted
    *  and has no zero-entries.
    */
  def subtract(x : Occurrences, y : Occurrences) : Occurrences = {
    def subset(m : Map[Char, Int], p : (Char, Int)) : Map[Char, Int] = {
      if (m(p._1) == p._2)
        m - p._1
      else
        m.updated(p._1, m(p._1) - p._2)
    }
    y.foldLeft(x.toMap)(subset).toList.sorted
  }

  /**
    * Returns a list of all anagram sentences of the given sentence.
    *
    *  An anagram of a sentence is formed by taking the occurrences of all the characters of
    *  all the words in the sentence, and producing all possible combinations of words with those characters,
    *  such that the words have to be from the dictionary.
    *
    *  The number of words in the sentence and its anagrams does not have to correspond.
    *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    *  Also, two sentences with the same words but in a different order are considered two different anagrams.
    *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    *  `List("I", "love", "you")`.
    *
    *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    *    List(
    *      List(en, as, my),
    *      List(en, my, as),
    *      List(man, yes),
    *      List(men, say),
    *      List(as, en, my),
    *      List(as, my, en),
    *      List(sane, my),
    *      List(Sean, my),
    *      List(my, en, as),
    *      List(my, as, en),
    *      List(my, sane),
    *      List(my, Sean),
    *      List(say, men),
    *      List(yes, man)
    *    )
    *
    *  The different sentences do not have to be output in the order shown above - any order is fine as long as
    *  all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    *  so it has to be returned in this list.
    *
    *  Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagramsNoCache(sentence : Sentence) : List[Sentence] = {
    def occurAnagrams(occur : Occurrences) : List[Sentence] = {
      if (occur == Nil) List(List())
      else
        for {
          elem ← combinations(occur) filter (dictionaryByOccurrences(_) != Nil)
          res ← occurAnagrams(subtract(occur, elem))
          word ← dictionaryByOccurrences(elem)
        } yield List(word) ::: res

    }
    occurAnagrams(sentenceOccurrences(sentence))
  }

  def sentenceAnagrams(sentence : Sentence) : List[Sentence] = {
    def occurrenceAnagrams(occur : Occurrences, m : Map[Occurrences, List[Sentence]]) : Map[Occurrences, List[Sentence]] = {

      def computeAnagrams(anagramMap : Map[Occurrences, List[Sentence]], occurElem : Occurrences) : Map[Occurrences, List[Sentence]] = {
        val subset = subtract(occur, occurElem)
        val sentencesOccurElem = dictionaryByOccurrences(occurElem) map (List(_))
        if (subset == List()) {
          if (anagramMap(occur) == Nil) anagramMap ++ Map(occur -> sentencesOccurElem)
          else anagramMap updated (occur, anagramMap(occur) ::: sentencesOccurElem)
        }
        else {
          val subsetMap = occurrenceAnagrams(subset, anagramMap)

          //     print(" Anagram: " + occurElem + " + " + subset)
          //     println()
          //     print("   Map: ")
          //     subsetMap.foreach(println)

          val subsetVal = subsetMap(subset)
          if (subsetVal != List(List())) {
            val resList = for {
              tailSent ← subsetVal
              headSent ← sentencesOccurElem
              if (tailSent != List())
            } yield headSent ::: tailSent
            if (subsetMap(occur) == Nil) subsetMap ++ Map(occur -> resList)
            else subsetMap updated (occur, subsetMap(occur) ::: resList)
          }
          else subsetMap
        }

      }
      // println("Occur: " + occur )

      if (occur != List() && m(occur) == Nil) {
        val comb = combinations(occur).filter(dictionaryByOccurrences(_) != List())
        //println(" Occurrences: " + comb)
        if (comb == Nil) m ++ Map(occur -> List(List()))
        else comb.foldLeft(m)(computeAnagrams)
      }
      else m

    }

    if (sentence == List()) List(List())
    else {
      val m : Map[Occurrences, List[Sentence]] = Map() withDefaultValue Nil
      val occur = sentenceOccurrences(sentence)
      val ret = occurrenceAnagrams(occur, m)
      // ret.foreach(println)
      ret(occur)
    }
  }

}
