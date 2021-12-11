import scala.io.Source
import scala.math.pow
import scala.util.{Random, Using}

object MinHashSimilarityCalculator {
  val hashingMax: Int = pow(2, 32).toInt

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

  def generateHashingFunctions(n: Int): Seq[(Int, Int)] =
    for (i <- 0 until n)
      yield (Random.nextInt(hashingMax), Random.nextInt(hashingMax))

  def getSignature(shingles: Set[String], hashingFunctions: Seq[(Int, Int)]): Seq[Int] =
    hashingFunctions.map(
      fn => shingles
        .map(shingle => (fn._1 * shingle.hashCode().abs + fn._2) % hashingMax)
        .min
    )

  def compareSignatures(sig1: Seq[Int], sig2: Seq[Int]): Double =
    sig1.zip(sig2).count { case (a, b) => a == b }.toDouble / sig1.length

  def main(args: Array[String]): Unit = {
    val book1 = "./src/crimeandpunishment.txt"
    val book2 = "./src/junglebook.txt"
    val hashingFunctions = generateHashingFunctions(100)

    for (k <- 4 until 13) {
      println(compareSignatures(
        getSignature(getKShingles(k, book1), hashingFunctions),
        getSignature(getKShingles(k, book2), hashingFunctions)
      ))
    }
  }
}
