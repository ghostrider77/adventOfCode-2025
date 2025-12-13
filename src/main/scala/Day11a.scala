object Day11a:
  class DirectedGraph(adjacencyList: Map[String, List[String]], startNode: String, endNode: String):
    private def countAllPaths(v: String): Int =
      if v == endNode then 1
      else
        val neighbors: List[String] = adjacencyList.getOrElse(v, Nil)
        neighbors.foldLeft(0)((acc, n) => acc + countAllPaths(n))

    def nrPaths: Int =
      countAllPaths(startNode)

  def parseInput(lines: List[String]): DirectedGraph =
    def parseLine(line: String): (String, List[String]) = line match
      case s"$start: $rest" => (start, rest.split(" ").toList)
      case _ => throw Exception(s"Malformed input $line.")

    DirectedGraph(lines.map(parseLine).toMap, "you", "out")

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input11.txt").getLines()
    val graph: DirectedGraph = parseInput(lines.toList)
    val result: Int = graph.nrPaths
    println(result)
