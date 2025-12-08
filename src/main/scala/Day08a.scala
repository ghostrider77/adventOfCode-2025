import scala.annotation.tailrec

object Day08a:
  import scala.collection.mutable.PriorityQueue as Heap

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

  given [T]: Ordering[(T, T, Double)] = Ordering.by(-_._3)

  private def distance(p1: Coord, p2: Coord): Double =
    math.sqrt(Iterator(p1.x - p2.x, p1.y - p2.y, p1.z - p2.z).foldLeft(0.0)((acc, d) => acc + math.pow(d, 2)))

  private def parseInput(lines: List[String]): List[Coord] =
    def parseLine(line: String): Coord = line match
      case s"$x,$y,$z" => Coord(x.toInt, y.toInt, z.toInt)
      case _ => throw Exception(s"Malformed input $line.")

    lines.map(parseLine)

  private def calcDistancePairs(positions: List[Coord], k: Int): List[(Int, Int, Double)] =
    val heap: Heap[(Int, Int, Double)] = Heap.empty[(Int, Int, Double)]
    for {
      (p1, i) <- positions.zipWithIndex
      (p2, j) <- positions.zipWithIndex.drop(i + 1)
      d = distance(p1, p2)
    } heap.enqueue((i, j, d))

    (0 until k).map(_ => heap.dequeue()).toList

  private def calcGroupSizes(nrPositions: Int, pairs: List[(Int, Int, Double)]): List[Int] =
    val circuits: UnionFind = UnionFind(nrPositions)
    pairs.foreach { (p, q, _) =>
      val groupOfP: Int = circuits.find(p)
      val groupOfQ: Int = circuits.find(q)
      circuits.union(groupOfP, groupOfQ)
    }

    val groupSizes: Map[Int, Int] = (0 until nrPositions).foldLeft(Map.empty[Int, Int]) {
      case (acc, ix) =>
        val root: Int = circuits.find(ix)
        acc.updated(root, acc.getOrElse(root, 0) + 1)
    }
    groupSizes.values.toList.sorted(using Ordering[Int].reverse)

  def calcClusterSizeProduct(positions: List[Coord], k: Int): Long =
    val closestPoints: List[(Int, Int, Double)] = calcDistancePairs(positions, k)
    val groupSizes: List[Int] = calcGroupSizes(positions.length, closestPoints)
    groupSizes.take(3).map(_.toLong).product

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input08.txt").getLines()
    val k: Int = 1000
    val positions: List[Coord] = parseInput(lines.toList)
    val result: Long = calcClusterSizeProduct(positions, k)
    println(result)
