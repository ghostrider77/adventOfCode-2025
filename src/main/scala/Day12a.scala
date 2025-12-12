object Day12a:
  case class Region(nrRows: Int, nrCols: Int)
  case class Puzzle(region: Region, shapeCounts: List[Int])

  private def parseInput(lines: List[String]): List[Puzzle] =
    def parsePuzzle(line: String): Puzzle = line match
      case s"${a}x$b: $counts" => Puzzle(Region(a.toInt, b.toInt), counts.split(" ").map(_.toInt).toList)
      case _ => throw Exception(s"Malformed input $line.")

    lines.reverseIterator.takeWhile(_.nonEmpty).map(parsePuzzle).toList

  def applyHeuristics(puzzles: List[Puzzle]): Int =
    puzzles.count { case Puzzle(Region(a, b), shapes) => shapes.sum * 9 <= a * b }

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input12.txt").getLines()
    val puzzles: List[Puzzle] = parseInput(lines.toList)
    val result: Int = applyHeuristics(puzzles)
    println(result)
