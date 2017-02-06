import forcomp.loadDictionary

type Word = String

type Sentence = List[Word]

//type Occurrences = Map[Char, String]
type Occurrences = List[(Char, Int)]

def wordOccurrences(w: Word): Occurrences = {
  w.groupBy((elem: Char) => elem.toLower).map((elem) => (elem._1, elem._2.length)).toList.sorted
}

// Test
//wordOccurrences("abcd")
//wordOccurrences("Robert")

def sentenceOccurrences(s: Sentence): Occurrences = s match {
  case Nil => Nil
  case List(x) => wordOccurrences(x)
  case head :: tail => wordOccurrences(head) ++ sentenceOccurrences(tail)
}

// Test
//sentenceOccurrences(List("abcd", "e"))

val dictionary: List[Word] = loadDictionary
lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
  dictionary.groupBy(word => wordOccurrences(word))
}

def wordAnagrams(word: Word): List[Word] = {
  dictionaryByOccurrences(wordOccurrences(word))
}

// Test
//dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet)
//wordAnagrams("married").toSet
//wordAnagrams("player").toSet

def subtract(x: Occurrences, y: Occurrences): Occurrences = y match {
  case List() => x
  case head :: tail =>
    (x take x.indexOf(head)) ::: (x drop x.indexOf(head) + 1)

}

def combinations(occurrences: Occurrences): List[Occurrences] = {
  occurrences.foldRight(List[Occurrences](Nil)) {
    case ((char, num), acc) =>
      acc ++ (for {
        comb <- acc
        charNum <- 1 to num
      } yield (char, charNum) :: comb)
  }
}

//val abba = List(('a', 2), ('b', 2))
//combinations(abba).toSet

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  def doSentenceAnagram(occurrences: Occurrences): List[Sentence] = occurrences match {
    case Nil => List(Nil)
    case _ =>
      for {
        comb <- combinations(occurrences)
        dict <- dictionaryByOccurrences.getOrElse(comb, Nil)
        sentenceAnagrams <- doSentenceAnagram(subtract(occurrences, comb))
      } yield List(dict) ++ sentenceAnagrams
  }
  doSentenceAnagram(sentenceOccurrences(sentence))
}

sentenceAnagrams(List("Linux", "rulez")).toSet