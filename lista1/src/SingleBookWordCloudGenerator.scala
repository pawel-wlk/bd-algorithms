import java.io.{File, PrintWriter}
import scala.collection.immutable.HashSet
import scala.io.Source
import scala.util.Using

object SingleBookWordCloudGenerator {
  def loadStopWords(path: String): HashSet[String] =
    Using(
      Source.fromFile(path)
    )(
      _.getLines().foldLeft(new HashSet[String])((accumulator, word) => accumulator + word)
    ).get

  def loadBookToWords(path: String, stopWords: HashSet[String]): Map[String, Int] =
    Using(
      Source.fromFile(path)
    )(
      _.getLines()
        .flatMap(
          _.replaceAll("""[“”\p{Punct}]""", "").split(" ")
        )
        .map(_.toLowerCase)
        .filter(word => word != "" && !stopWords.contains(word))
        .foldLeft(Map[String, Int]())(
          (accumulator, word) => accumulator + (word -> (accumulator.getOrElse(word, 0) + 1))
        )
    ).get

  def main(args: Array[String]): Unit = {
    val pw = new PrintWriter(new File("data/word_cloud.csv"))

    for ((word, count) <- loadBookToWords("data/book.txt", loadStopWords("data/stop_words_english.txt")).toSeq.sortBy(-_._2).take(30)) {
      pw.write(count + ";" + word + "\n")
    }

    pw.close()
  }
}
