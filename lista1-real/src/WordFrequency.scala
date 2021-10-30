import scala.Predef.->
import scala.collection.immutable.HashSet
import scala.io.Source
import scala.util.Using

object WordFrequency {
  def loadStopWords(path: String): HashSet[String] =
    Using(Source.fromFile(path))(
      _.getLines().foldLeft(new HashSet[String])((accumulator, word) => accumulator + word)
    ).get

  def transformBookToWordMap(book: String, stopWords: HashSet[String]): Map[String, Int] =
    book.replaceAll("""[\p{Punct}]""", "").split("\\s")
      .map(_.toLowerCase)
      .filter(word => word != "" && !stopWords.contains(word))
      .foldLeft(Map[String, Int]())(
        (accumulator, word) => accumulator + (word -> (accumulator.getOrElse(word, 0) + 1))
      )

  def loadBookToString(path: String): String =
    Using(Source.fromFile(path))(_.mkString).get

  def transformWordMapToCsvSeq[T](wordMap: Map[String, T], numberOfWordsForCloud: Int)(implicit num: Numeric[T]): Seq[String] = {
    import num._
    wordMap
      .toSeq
      .sortBy(-_._2)
      .take(numberOfWordsForCloud)
      .map { case (word, count) => count + ";" + word }
  }

  def printSeqToScreen(seq: Seq[String]): Unit = {
    for (str <- seq) {
      println(str)
    }
  }

  def termFrequency(word: String, wordMap: Map[String, Int]): Double =
    wordMap.getOrElse(word, 0) / wordMap.foldLeft(0)(_ + _._2)

  def inverseDocumentFrequency(word: String, allWordMaps: Array[Map[String, Int]]): Double =
    math.log(allWordMaps.length / allWordMaps.count(_.contains(word)))

  def tfIdf(word: String, wordMap: Map[String, Int], allWordMaps: Array[Map[String, Int]]): Double =
    termFrequency(word, wordMap) * inverseDocumentFrequency(word, allWordMaps)

  def loadAllBooks(paths: Array[String]): Map[String, String] =
    paths.foldLeft(Map[String, String]())((accumulator, path) => accumulator + (path -> loadBookToString(path)))

  def calculateFrequentWordsPerBook(bookMap: Map[String, String], stopWords: HashSet[String]): Map[String, Map[String, Int]] =
    bookMap.mapValues(transformBookToWordMap(_, stopWords)).toMap

  def calculateTfIdfsForOneBook(wordMap: Map[String, Int], allWordMaps: Array[Map[String, Int]]): Map[String, Double] =
    wordMap.toSeq
      .map { case (word, count) => (word, tfIdf(word, wordMap, allWordMaps)) }
      .foldLeft(Map[String, Double]())((accumulator, pair) => accumulator + (pair._1 -> pair._2))

  def calculateTfIdfsPerBook(bookWordsMap: Map[String, Map[String, Int]]): Map[String, Map[String, Double]] = {
    val allWordMaps = bookWordsMap.values.toArray

    bookWordsMap.mapValues(calculateTfIdfsForOneBook(_, allWordMaps)).toMap
  }

  def main(args: Array[String]): Unit = {
    val stopWords = loadStopWords("data/stopwords.txt")
    val books = Array("xd", "test")

    val loadedBooks = loadAllBooks(books)
    val frequentWordsPerBook = calculateFrequentWordsPerBook(loadedBooks, stopWords)
    val tfIdfsPerBook = calculateTfIdfsPerBook(frequentWordsPerBook)

    for (book <- books) {
      println(book)
      printSeqToScreen(transformWordMapToCsvSeq(frequentWordsPerBook.getOrElse(book, Map()), 10))
      printSeqToScreen(transformWordMapToCsvSeq(tfIdfsPerBook.getOrElse(book, Map()), 10))
    }

  }
}
