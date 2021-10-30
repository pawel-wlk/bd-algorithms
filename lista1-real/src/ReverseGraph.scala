object ReverseGraph {
  def reverseGraph(graph: Array[(Int, Array[Int])]): Array[(Int, Array[Int])] =
    graph
      .flatMap { case (node, neighbors) => neighbors.map((node, _)) } // divide problem to smaller subproblems (map)
      .map { case (node, neighbor) => (neighbor, node) } // solve smaller subproblems
      .foldLeft(Map[Int, Array[Int]]())((graph, edge) => graph +
        (edge._1 -> graph.getOrElse(edge._1, Array()).appended(edge._2))) // join solutions (reduce)
      .toArray

  def main(args: Array[String]): Unit = {
    val graph = Array(
      (1, Array(2,3)),
      (3, Array(1, 5)),
      (2, Array(5)),
      (5, Array(): Array[Int])
    )

    val reversed = reverseGraph(graph)

    for (node <- reversed) {
      println(s"(${node._1}, [${node._2.mkString(",")}])")
    }
  }
}
