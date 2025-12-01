object Day01b:
  import scala.annotation.tailrec
  import scala.math.Integral.Implicits._
  import Direction.{Left, Right}

  private val DialSize: Int = 100

  enum Direction:
    case Left, Right

  object Direction:
    def apply(c: Char): Direction = c match
      case 'L' => Left
      case 'R' => Right
      case _ => throw Exception(s"Unknown direction $c")

  case class Rotation(step: Int, direction: Direction)

  def parseInput(line: String): Rotation =
    Rotation(line.substring(1).toInt, Direction(line(0)))

  def countTotalZeroPositionPasses(startPosition: Int, rotations: List[Rotation]): Int =
    @tailrec
    def loop(acc: Int, rs: List[Rotation], currentPosition: Int): Int = rs match
      case Nil => acc
      case Rotation(step, direction) :: rest =>
        val nextPosition: Int = direction match
          case Left => currentPosition - step
          case Right => currentPosition + step
        val (k, remainder) = nextPosition /% DialSize
        val updatedNextPosition: Int = if remainder < 0 then remainder + DialSize else remainder
        if currentPosition == 0 || direction == Right then loop(acc + math.abs(k), rest, updatedNextPosition)
        else if remainder > 0 then loop(acc, rest, updatedNextPosition)
        else loop(acc + math.abs(k) + 1, rest, updatedNextPosition)

    loop(0, rotations, startPosition)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input01.txt").getLines()
    val rotations: List[Rotation] = lines.map(parseInput).toList
    val result: Int = countTotalZeroPositionPasses(50, rotations)
    println(result)
