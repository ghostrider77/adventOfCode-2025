object Day09b:
  case class Coord(x: Long, y: Long)

  private def area(c1: Coord, c2: Coord): Long =
    (math.abs(c1.x - c2.x) + 1) * (math.abs(c1.y - c2.y) + 1)

  private def parseInput(lines: List[String]): List[Coord] =
    def parseLine(line: String): Coord = line match
      case s"$x,$y" => Coord(x.toInt, y.toInt)
      case _ => throw Exception(s"Malformed input $line.")

    lines.map(parseLine)

  def calcLargestAreaInsidePolygon(coords: List[Coord]): Long =
    val sides: List[(Coord, Coord)] =
      (coords.last :: coords)
        .sliding(2)
        .collect { case List(Coord(x, y), Coord(u, v)) => (Coord(x min u, y min v), Coord(x max u, y max v)) }
        .toList

    coords
      .combinations(2)
      .collect { case List(Coord(x1, y1), Coord(x2, y2)) => (Coord(x1 min x2, y1 min y2), Coord(x1 max x2, y1 max y2)) }
      .foldLeft(0L) { case (acc, (c1 @ Coord(x, y), c2 @ Coord(u, v))) =>
        if sides.exists { case (Coord(a, b), Coord(c, d)) => a < u && b < v && c > x && d > y } then acc
        else acc max area(c1, c2)
      }

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input09.txt").getLines()
    val coords: List[Coord] = parseInput(lines.toList)
    val result: Long = calcLargestAreaInsidePolygon(coords)
    println(result)
