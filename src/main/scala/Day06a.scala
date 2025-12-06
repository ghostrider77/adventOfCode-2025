object Day06a:
  import Operation.*

  enum Operation:
    case Add, Mul

  object Operation:
    def apply(s: String): Operation = s match
      case "+" => Add
      case "*" => Mul
      case _ => throw Exception(s"Unknown operator $s.")

  case class MathProblem(numbers: List[Long], operation: Operation):
    private val operator: (Long, Long) => Long = operation match
      case Add => _ + _
      case Mul => _ * _

    val value: Long = (numbers, operation) match
      case (Nil, Add) => 0L
      case (Nil, Mul) => 1L
      case (hd :: tl, _) => tl.foldLeft(hd)(operator)

  private def parseInput(lines: List[String]): List[MathProblem] =
    def readProblem(items: List[String]): MathProblem =
      MathProblem(items.init.map(_.toLong), Operation(items.last))

    lines.map(_.strip().split("\\s+")).transpose.map(readProblem)

  def calcGrandTotal(problems: List[MathProblem]): Long =
    problems.foldLeft(0L)((acc, p) => acc + p.value)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input06.txt").getLines()
    val problems: List[MathProblem] = parseInput(lines.toList)
    val result: Long = calcGrandTotal(problems)
    println(result)
