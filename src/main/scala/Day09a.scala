object Day09a:
  case class Coord(x: Long, y: Long)

  private def area(c1: Coord, c2: Coord): Long =
    (math.abs(c1.x - c2.x) + 1) * (math.abs(c1.y - c2.y) + 1)

  private def parseInput(lines: List[String]): List[Coord] =
    def parseLine(line: String): Coord = line match
      case s"$x,$y" => Coord(x.toInt, y.toInt)
      case _ => throw Exception(s"Malformed input $line.")

    lines.map(parseLine)

  def calcLargestArea(coords: List[Coord]): Long =
    coords
      .combinations(2)
      .collect { case List(c1, c2) => (c1, c2) }
      .foldLeft(0L) { case (acc, (c1, c2)) => math.max(acc, area(c1, c2)) }

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input09.txt").getLines()
    val coords: List[Coord] = parseInput(lines.toList)
    val result: Long = calcLargestArea(coords)
    println(result)
