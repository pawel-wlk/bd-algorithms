import java.io.{File, PrintWriter}
import scala.collection.immutable.HashSet
import scala.io.{Source, StdIn}
import scala.util.Using
import scala.util.control.Breaks._

object WordCloudGenerator {
  def loadStopWords(path: String): HashSet[String] =
    Using(Source.fromFile(path))(
      _.getLines().foldLeft(new HashSet[String])((accumulator, word) => accumulator + word)
    ).get

  def transformBookToWordMap(book: String, stopWords: HashSet[String]): Map[String, Int] =
    book.replaceAll("""[“”\p{Punct}]""", "").split("\\s")
      .map(_.toLowerCase)
      .filter(word => word != "" && !stopWords.contains(word))
      .foldLeft(Map[String, Int]())(
        (accumulator, word) => accumulator + (word -> (accumulator.getOrElse(word, 0) + 1))
      )

  def loadBookToString(path: String): String =
    Using(Source.fromFile(path))(_.mkString).get

  def transformWordMapToWordCloudCsvSeq(wordMap: Map[String, Int], numberOfWordsForCloud: Int): Seq[String] =
    wordMap
      .toSeq
      .sortBy(-_._2)
      .take(numberOfWordsForCloud)
      .map { case (word, count) => count + ";" + word }

  def writeSeqToFile(seq: Seq[String], path: String): Unit = {
    val pw = new PrintWriter(new File(path))

    for (str <- seq) {
      pw.write(str + "\n")
    }

    pw.close()
  }

  def printSeqToScreen(seq: Seq[String]): Unit = {
    for (str <- seq) {
      println(str)
    }
  }

  def main(args: Array[String]): Unit = {
    val stopWords = loadStopWords("data/stop_words_english.txt")

    var isWorking = true
    var answer = ""

    while (isWorking) {
      breakable {
        try {
          println("Welcome to word cloud generator, type exit to quit")
          println("Would you like to parse a (s)tring or a (f)ile?")

          answer = StdIn.readLine("")

          val book = if (answer == "string" || answer == "s") {
            println("type what you want and press enter")
            StdIn.readLine()
          } else if (answer == "file" || answer == "f") {
            println("specify a path")
            loadBookToString(StdIn.readLine())
          } else {
            println("incorrect answer :(")
            break
          }

          val bookWordMap = transformBookToWordMap(book, stopWords)

          println("How many words would you like in your word cloud?")

          val numberOfWords = StdIn.readInt()
          val wordCloudSeq = transformWordMapToWordCloudCsvSeq(bookWordMap, numberOfWords)

          println("Would you like to print the output to the (c)onsole or to a (f)ile?")
          answer = StdIn.readLine("")

          if (answer == "console" || answer == "c") {
            printSeqToScreen(wordCloudSeq)
          } else if (answer == "file" || answer == "f") {
            println("Please, provide a path")
            writeSeqToFile(wordCloudSeq, StdIn.readLine())
          } else {
            println("incorrect answer :(")
            break
          }

          println("Would you like to try one more time? (y)es or (n)o")
          answer = StdIn.readLine()

          if (answer == "yes" || answer == "y") {
            println("great!")
          } else if (answer == "no" || answer == "n") {
            println("ok, bye")
            isWorking = false
          } else {
            println("I don't know what you mean but let's try again!")
          }
        } catch {
          case _: NumberFormatException => println("Your number is not a number")
          case _: Any => println("Something went wrong")
        }
      }
    }
  }
}
