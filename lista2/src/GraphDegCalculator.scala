import scala.io.Source
import scala.util.Using


case class Vertex(
                   inDeg: Int,
                   outDeg: Int
                 ) {
  override def toString: String = s"(inDeg: $inDeg, outDeg: $outDeg)"
}

object GraphDegCalculator {
  def mapFunction(line: String): Array[(Int, Vertex)] = {
    val Array(v, u) = line.split("\\s+").map(_.toInt)

    Array((v, Vertex(0, 1)), (u, Vertex(1, 0)))
  }

  def reduceFunction(acc: Map[Int, Vertex], vertexTuple: (Int, Vertex)): Map[Int, Vertex] = {
    val (label, vertex) = vertexTuple
    if (acc.contains(label)) {
      acc + (label -> Vertex(acc(label).inDeg + vertex.inDeg, acc(label).outDeg + vertex.outDeg))
    } else {
      acc + (label -> vertex)
    }
  }

  def main(args: Array[String]): Unit = {
    Using(Source
      .fromFile(args(0))) {
      source =>
        println(source.getLines()
          .flatMap(mapFunction)
          .foldLeft(Map[Int, Vertex]())(reduceFunction)
        )
    }
  }
}
