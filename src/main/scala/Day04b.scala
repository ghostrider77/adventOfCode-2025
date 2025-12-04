import scala.annotation.tailrec

object Day04b:
  enum Cell:
    case Empty, PaperRoll

  object Cell:
    def apply(c: Char): Cell = c match
      case '.' => Empty
      case '@' => PaperRoll
      case _ => throw Exception(s"Unknown cell $c.")

  case class Coord(x: Int, y: Int)

  case class Department(rolls: Set[Coord], nrRows: Int, nrCols: Int):
    private def isValid(coord: Coord): Boolean =
      0 <= coord.x && coord.x < nrRows && 0 <= coord.y && coord.y < nrCols

    def getNumberOfRollNeighbors(coord: Coord): Int =
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
      neighborCoord.count(rolls.contains)

  def parseInput(lines: List[String]): Department = lines match
    case Nil => throw Exception("Empty input.")
    case hd :: _ =>
      val nrCols: Int = hd.length
      val nrRows = lines.length
      val rolls =
        lines
          .zipWithIndex
          .flatMap { case (line, x) =>
            line.zipWithIndex.collect { case (c, y) if Cell(c) == Cell.PaperRoll => Coord(x, y) }
          }
          .toSet
      Department(rolls, nrRows, nrCols)

  def countRemovableRolls(department: Department): Int =
    @tailrec
    def loop(acc: Int, dept: Department): Int =
      val Department(rolls, _, _) = dept
      val removableRolls: Set[Coord] = rolls.filter(dept.getNumberOfRollNeighbors(_) < 4)
      val size: Int = removableRolls.size
      if size == 0 then acc
      else
        val updatedDept: Department = dept.copy(rolls = rolls -- removableRolls)
        loop(acc + size, updatedDept)

    loop(0, department)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input04.txt").getLines()
    val dept: Department = parseInput(lines.toList)
    val result: Int = countRemovableRolls(dept)
    println(result)
