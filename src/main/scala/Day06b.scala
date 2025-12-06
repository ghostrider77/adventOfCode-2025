import scala.annotation.tailrec

object Day06b:
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

    val value: Long = numbers match
      case Nil => if operator == Add then 0L else 1L
      case hd :: tl => tl.foldLeft(hd)(operator)

  private def parseInput(lines: List[String]): List[MathProblem] =
    @tailrec
    def loop(acc: List[MathProblem], ns: List[String], ops: List[Operation]): List[MathProblem] = (ns, ops) match
      case (Nil, Nil) => acc
      case (_, op :: restOp) =>
        val (numbers, restNs) = ns.span(_.nonEmpty)
        val problem = MathProblem(numbers.map(_.toLong), op)
        restNs match
          case Nil => loop(problem :: acc, Nil, restOp)
          case _ :: tl => loop(problem :: acc, tl, restOp)
      case _ => throw Exception("Malformed input.")

    val maxLength: Int = lines.foldLeft(0)((acc, line) => math.max(acc, line.length))
    val paddedLines: List[String] = lines.map(_.padTo(maxLength, ' '))
    val operations: List[Operation] = lines.last.strip().split("\\s+").map(Operation(_)).toList
    val numberLines: List[String] = paddedLines.init.map(_.toCharArray).transpose.map(_.mkString.strip())
    loop(Nil, numberLines, operations)

  def calcGrandTotal(problems: List[MathProblem]): Long =
    problems.foldLeft(0L)((acc, p) => acc + p.value)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input06.txt").getLines()
    val problems: List[MathProblem] = parseInput(lines.toList)
    val result: Long = calcGrandTotal(problems)
    println(result)
