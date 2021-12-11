import scala.io.Source
import scala.util.Using

object DocumentSimilarityCalculator {
  def getKShingles(k: Int, book: String): Set[String] =
    Using(Source.fromFile(book)) { source => {
      val tokens = source
        .getLines()
        .reduce(_ ++ " " ++ _)
        .replaceAll("""[“”\p{Punct}]""", "")
        .split(" ")
        .map(_.toLowerCase)


      (for (i <- 0 until tokens.length - k + 1)
        yield tokens.slice(i, i + k).mkString(" ")
        ).toSet
    }
    }.get

  def jaccardSimilarity(set1: Set[String], set2: Set[String]): Double =
    if (set1.isEmpty || set2.isEmpty) 1
    else set1.intersect(set2).size.toDouble / set1.union(set2).size

  def main(args: Array[String]): Unit = {
    val book1 = "./src/crimeandpunishment.txt"
    val book2 = "./src/junglebook.txt"

    for (k <- 4 until 13) {
      println(jaccardSimilarity(getKShingles(k, book1), getKShingles(k, book2)))
    }
  }
}
