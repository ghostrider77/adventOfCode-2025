import scala.annotation.tailrec

object Day05b:
  case class IDRange(start: Long, end: Long)

  private def parseInput(lines: List[String]): List[IDRange] =
    def parseRange(line: String): IDRange = line match
      case s"$a-$b" => IDRange(a.toLong, b.toLong)
      case _ => throw Exception(s"Malformed input $line.")

    val ids: List[String] = lines.takeWhile(_ != "")
    ids.map(parseRange)

  def getNrOfAllFreshIDs(idRanges: List[IDRange]): Long =
    @tailrec
    def mergeRanges(acc: List[IDRange], currentRange: IDRange, ranges: List[IDRange]): List[IDRange] = ranges match
      case Nil => currentRange :: acc
      case (r @ IDRange(start, end)) :: rs =>
        if start <= currentRange.end then mergeRanges(acc, currentRange.copy(end = end max currentRange.end), rs)
        else mergeRanges(currentRange :: acc, r, rs)

    idRanges.sortBy(_.start) match
      case Nil => 0
      case hd :: tl =>
        val disjointRanges: List[IDRange] = mergeRanges(Nil, hd, tl)
        disjointRanges.foldLeft(0L) { case (acc, IDRange(a, b)) => acc + (b - a + 1) }

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input05.txt").getLines()
    val idRanges: List[IDRange] = parseInput(lines.toList)
    val result: Long = getNrOfAllFreshIDs(idRanges)
    println(result)
