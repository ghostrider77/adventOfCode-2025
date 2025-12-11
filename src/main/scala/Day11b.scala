import scala.annotation.tailrec

object Day11b:
  import scala.collection.mutable.Queue as MutableQueue

  class DirectedGraph(adjacencyList: Map[String, List[String]]):
    private val reversedEdges: Map[String, List[String]] =
      (for {
        (node, neighbors) <- adjacencyList.toList
        neighbor <- neighbors
      } yield (neighbor, node)).groupMap { case (node, _) => node } { case (_, neighbors) => neighbors }

    private def canReachNode(node: String): Set[String] =
      val queue: MutableQueue[String] = MutableQueue.from(List(node))

      @tailrec
      def loop(visited: Set[String]): Set[String] =
        if queue.isEmpty then visited
        else
          val u: String = queue.dequeue()
          val ancestors: List[String] = reversedEdges.getOrElse(u, Nil)
          val unvisitedAncestors: List[String] = ancestors.filterNot(visited.contains)
          queue.enqueueAll(unvisitedAncestors)
          loop(visited ++ unvisitedAncestors)

      loop(Set(node))

    def findAllPaths(startNode: String, targetNode: String): List[List[String]] =
      val nodesThatCanReachTarget: Set[String] = canReachNode(targetNode)

      def extendFrom(v: String): List[List[String]] =
        if v == targetNode then List(List(targetNode))
        else
          val neighbors: List[String] = adjacencyList.getOrElse(v, Nil)
          neighbors.filter(nodesThatCanReachTarget.contains).flatMap(n => extendFrom(n)).map(v :: _)

      if !nodesThatCanReachTarget.contains(startNode) then Nil
      else extendFrom(startNode)

  def parseInput(lines: List[String]): DirectedGraph =
    def parseLine(line: String): (String, List[String]) = line match
      case s"$start: $rest" => (start, rest.split(" ").toList)
      case _ => throw Exception(s"Malformed input $line.")

    DirectedGraph(lines.map(parseLine).toMap)

  def findNumberOfPaths(graph: DirectedGraph): Long =
    // There is no directed path from dac to fft
    val svrToFft: Long = graph.findAllPaths("svr", "fft").length
    val fftToDac: Long = graph.findAllPaths("fft", "dac").length
    val dacToOut: Long = graph.findAllPaths("dac", "out").length
    svrToFft * fftToDac * dacToOut

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input11.txt").getLines()
    val graph: DirectedGraph = parseInput(lines.toList)
    val result: Long = findNumberOfPaths(graph)
    println(result)
