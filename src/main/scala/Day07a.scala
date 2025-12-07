import scala.annotation.tailrec

object Day07a:
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

  def getNrOfsplits(manifold: Manifold): Int =
    val Manifold(start, splitters, nrRows, nrCols) = manifold
    def isValid(coord: Coord): Boolean =
      val Coord(x, y) = coord
      0 <= x && x < nrRows && 0 <= y && y < nrCols

    @tailrec
    def loop(acc: Int, beamHeads: Set[Coord]): Int = {
      val nrNextSplitters: Int = beamHeads.count { case Coord(x, y) => splitters.contains(Coord(x + 1, y)) }
      val nextHeads: Set[Coord] =
        beamHeads
          .flatMap { case Coord(x, y) =>
            if splitters.contains(Coord(x + 1, y)) then Set(Coord(x + 1, y - 1), Coord(x + 1, y + 1))
            else Set(Coord(x + 1, y))
          }
          .filter(isValid)
      if nextHeads.isEmpty then acc
      else loop(acc + nrNextSplitters, nextHeads)
    }

    loop(0, Set(start))

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input07.txt").getLines()
    val manifold: Manifold = parseInput(lines.toList)
    val result: Int = getNrOfsplits(manifold)
    println(result)
