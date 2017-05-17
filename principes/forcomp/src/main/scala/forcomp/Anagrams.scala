package forcomp

import common._

import scala.collection.immutable._
object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
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

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase.groupBy(x => x).mapValues(x => x.length).toList.sorted
  /*{
    val interMap:List[(Int,String)]=w.toLowerCase groupBy((elem:Char)=>w.toLowerCase.count((c:Char)=>c==elem)) toList
    for(
      (i,s)<-interMap;c<-s
    )yield (c,i)
  }*/

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences( s.mkString )

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
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
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy(w=>wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))
  /** Returns the list of all subsets of the occurrence list.
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
    *  Occurrences = List[(Char, Int)]
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def it(t:Tuple2[Char,Int])=(for(i<-1 to t._2)yield(t._1,i)).toList
    def genAllList(l1:List[Occurrences],l2:Occurrences):List[Occurrences]=(for(i <- l1; j <- l2) yield (j::i))
    /*
    *function val subsets
    *param: occurences as o
    *Step 1: o=>Map(tuple2 t=>it(t))
    * return: List[Occurrences]
    */
    val subsets:List[Occurrences]=occurrences.map( x => it(x))
    subsets.foldRight(List[Occurrences](Nil))((x,acc)=>acc ++ (genAllList(acc,x)))
  }




  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *  Hint: you can use `foldLeft`, and `-`, `apply` and `updated` operations on `Map`.
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val yMap=y.toMap.withDefault(_=>0)
    val xMap=x.toMap.withDefault(_=>0)
    def XfilterY(acc:Map[Char,Int],t:Tuple2[Char,Int]):Map[Char,Int]= acc.updated(t._1,acc(t._1)-yMap(t._1))
    xMap.foldLeft(xMap){(acc,occ)=>XfilterY(acc,occ)}.toList.filter{case (k,v)=>v>0}.sortBy(_._1)
  }

  /** Returns a list of all anagram sentences of the given sentence.
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
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    lazy val o=sentenceOccurrences(sentence)
    def sentenceAnagramsInner(o: Occurrences): List[Sentence] = {
      if (o.isEmpty) {
        List(Nil)
      } else {
        val combs = combinations(o)
        for (i <- combs if dictionaryByOccurrences.keySet(i);
             j <- dictionaryByOccurrences(i);
             s <- sentenceAnagramsInner(subtract(o, i))) yield {j :: s}
      }
    }
      sentenceAnagramsInner(o)
  }
}
object Main extends App {
import Anagrams._

  println(wordOccurrences("Robert").toString)
  println((for((c, n) <- wordOccurrences("Robert");m <- 1 to n)yield (c,m))).toString
  println(wordOccurrences("Robert").map( x => (for(i <- 1 until (x._2+1)) yield (x._1,i)).toList).toString)
println(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet).toString)
  println(wordAnagrams("married").toSet.toString)
  val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
  val r = List(('r', 1))
  val lad = List(('a', 1), ('d', 1), ('l', 1))
  println(subtract(lard, r).toString)
}