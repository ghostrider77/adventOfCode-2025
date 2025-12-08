import scala.annotation.tailrec

object Day08b:
  class UnionFind(nrPoints: Int):
    private val parentIndices: Array[Int] = (0 until nrPoints).toArray
    private val ranks: Array[Int] = Array.fill(nrPoints)(0)

    private def changeParentsToRoot(indicesOnPath: List[Int], root: Int): Unit =
      indicesOnPath.foreach(ix => parentIndices(ix) = root)

    def find(childIndex: Int): Int =
      @tailrec
      def loop(id: Int, parentId: Int, indicesTowardsRoot: List[Int]): (Int, List[Int]) =
        if id == parentId then (id, indicesTowardsRoot)
        else loop(parentId, parentIndices(parentId), id :: indicesTowardsRoot)

      val (root, indicesOnPath): (Int, List[Int]) = loop(childIndex, parentIndices(childIndex), Nil)
      changeParentsToRoot(indicesOnPath, root)
      root

    def union(parentIndexP: Int, parentIndexQ: Int): Unit =
      if parentIndexP != parentIndexQ then
        if ranks(parentIndexP) > ranks(parentIndexQ) then parentIndices(parentIndexQ) = parentIndexP
        else
          parentIndices(parentIndexP) = parentIndexQ
          if ranks(parentIndexP) == ranks(parentIndexQ) then ranks(parentIndexQ) += 1
  end UnionFind

  case class Coord(x: Int, y: Int, z: Int)

  private def distance(p1: Coord, p2: Coord): Double =
    math.sqrt(Iterator(p1.x - p2.x, p1.y - p2.y, p1.z - p2.z).foldLeft(0.0)((acc, d) => acc + math.pow(d, 2)))

  private def parseInput(lines: List[String]): List[Coord] =
    def parseLine(line: String): Coord = line match
      case s"$x,$y,$z" => Coord(x.toInt, y.toInt, z.toInt)
      case _ => throw Exception(s"Malformed input $line.")

    lines.map(parseLine)

  private def calcDistancePairs(positions: List[Coord]): List[(Int, Int, Double)] =
    (for {
      (p1, i) <- positions.zipWithIndex
      (p2, j) <- positions.zipWithIndex.drop(i + 1)
      d = distance(p1, p2)
    } yield (i, j, d)).sortBy(_._3)

  def createSingleCluster(positions: List[Coord]): Long =
    val nrPoints: Int = positions.length
    val distances: List[(Int, Int, Double)] = calcDistancePairs(positions)
    val circuits = new UnionFind(nrPoints)

    @tailrec
    def loop(nrClusters: Int, ds: List[(Int, Int, Double)]): Long = ds match
      case Nil => throw Exception("All points were considered, still not clustered.")
      case (ixP, ixQ, dist) :: rest =>
        val clusterOfP: Int = circuits.find(ixP)
        val clusterOfQ: Int = circuits.find(ixQ)
        if (clusterOfP == clusterOfQ) loop(nrClusters, rest)
        else
          circuits.union(clusterOfP, clusterOfQ)
          if nrClusters == 2 then
            val p: Coord = positions(ixP)
            val q: Coord = positions(ixQ)
            p.x.toLong * q.x.toLong
          else loop(nrClusters - 1, rest)

    loop(nrPoints, distances)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input08.txt").getLines()
    val positions: List[Coord] = parseInput(lines.toList)
    val result: Long = createSingleCluster(positions)
    println(result)
