object Day01a:
  import scala.annotation.tailrec
  import Direction.{Left, Right}

  private val DialSize: Int = 100

  extension (k: Int)
    private def modulo(n: Int): Int =
      val r: Int = k % n
      if r < 0 then r + DialSize else r

  enum Direction:
    case Left, Right

  object Direction:
    def apply(c: Char): Direction = c match
      case 'L' => Left
      case 'R' => Right
      case _ => throw Exception(s"Unknown direction $c")

  case class Rotation(step: Int, direction: Direction):
    def apply(startPosition: Int): Int = direction match
      case Left => (startPosition - step).modulo(DialSize)
      case Right => (startPosition + step).modulo(DialSize)

  def parseInput(line: String): Rotation =
    Rotation(line.substring(1).toInt, Direction(line(0)))

  def countZeroPositions(startPosition: Int, rotations: List[Rotation]): Int =
    @tailrec
    def loop(acc: Int, rs: List[Rotation], currentPosition: Int): Int = rs match
      case Nil => acc
      case rotation :: rest =>
        val nextPosition: Int = rotation(currentPosition)
        if nextPosition == 0 then loop(acc + 1, rest, nextPosition) else loop(acc, rest, nextPosition)

    loop(0, rotations, startPosition)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input01.txt").getLines()
    val rotations: List[Rotation] = lines.map(parseInput).toList
    val result: Int = countZeroPositions(50, rotations)
    println(result)
