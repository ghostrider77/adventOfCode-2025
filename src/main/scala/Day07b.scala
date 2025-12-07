object Day07b:
  case class Coord(x: Int, y: Int)
  case class Manifold(start: Coord, splitters: Set[Coord], nrRows: Int, nrCols: Int)

  private def parseInput(lines: List[String]): Manifold = lines match
    case Nil => throw Exception("Empty input.")
    case hd :: _ =>
      val nrCols: Int = hd.length
      val nrRows: Int = lines.length
      val cells: List[(Coord, Char)] =
        lines.zipWithIndex.flatMap { case (line, x) => line.zipWithIndex.map { case (c, y) => (Coord(x, y), c) } }
      val start: Coord = cells.find { case (_, c) => c == 'S' } match
        case None => throw Exception("No start cell was found.")
        case Some((coord, _)) => coord
      val splitters: Set[Coord] = cells.collect { case (coord, c) if c == '^' => coord }.toSet
      Manifold(start, splitters, nrRows, nrCols)

  def getNrOfParticlePaths(manifold: Manifold): Long =
    val Manifold(start, splitters, nrRows, nrCols) = manifold
    val nrPaths: Array[Array[Long]] = Array.fill(nrRows, nrCols)(0L)
    nrPaths(start.x)(start.y) = 1L
    for {
      x <- 1 until nrRows
      y <- 0 until nrCols
    } {
      if !splitters.contains(Coord(x, y)) then {
        if splitters.contains(Coord(x, y - 1)) then nrPaths(x)(y) += nrPaths(x - 1)(y - 1)
        if splitters.contains(Coord(x, y + 1)) then nrPaths(x)(y) += nrPaths(x - 1)(y + 1)
        nrPaths(x)(y) += nrPaths(x - 1)(y)
      }
    }

    nrPaths(nrRows - 1).sum

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input07.txt").getLines()
    val manifold: Manifold = parseInput(lines.toList)
    val result: Long = getNrOfParticlePaths(manifold)
    println(result)
