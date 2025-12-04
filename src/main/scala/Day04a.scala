object Day04a:
  enum Cell:
    case Empty, PaperRoll

  object Cell:
    def apply(c: Char): Cell = c match
      case '.' => Empty
      case '@' => PaperRoll
      case _ => throw Exception(s"Unknown cell $c.")

  case class Coord(x: Int, y: Int)
  case class Department(room: Vector[Vector[Cell]], nrRows: Int, nrCols: Int):
    private def isValid(coord: Coord): Boolean =
      0 <= coord.x && coord.x < nrRows && 0 <= coord.y && coord.y < nrCols

    def getCell(coord: Coord): Cell =
      val Coord(x, y) = coord
      room(x)(y)

    def getNeighbors(coord: Coord): List[Cell] =
      val Coord(x, y) = coord
      val neighborCoord: List[Coord] =
        Iterator(
          Coord(x + 1, y - 1),
          Coord(x + 1, y),
          Coord(x + 1, y + 1),
          Coord(x, y + 1),
          Coord(x - 1, y + 1),
          Coord(x - 1, y),
          Coord(x - 1, y - 1),
          Coord(x, y - 1)
        ).filter(isValid).toList
      neighborCoord.map { case Coord(x, y) => room(x)(y) }

  def parseInput(lines: List[String]): Department = lines match
    case Nil => throw Exception("Empty input.")
    case hd :: _ =>
      val nrCols: Int = hd.length
      val nrRows = lines.length
      val room: Vector[Vector[Cell]] = lines.map(_.toVector.map(Cell(_))).toVector
      Department(room, nrRows, nrCols)

  def countAccessableRolls(dept: Department): Int =
    val Department(_, nrRows, nrCols) = dept
    (0 until nrRows)
      .flatMap(x => (0 until nrCols).map(y => Coord(x, y)))
      .filter(dept.getCell(_) == Cell.PaperRoll)
      .count(dept.getNeighbors(_).count(_ == Cell.PaperRoll) < 4)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input04.txt").getLines()
    val dept: Department = parseInput(lines.toList)
    val result: Int = countAccessableRolls(dept)
    println(result)
