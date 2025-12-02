object Day02a:
  case class IdRange(start: Long, end: Long)

  private def parseRanges(line: String): List[IdRange] =
    def parse(item: String): IdRange = item match
      case s"$start-$end" => IdRange(start.toLong, end.toLong)
      case _ => throw Exception(s"Invalid range $item.")

    line.split(",").map(parse).toList

  private def isIdInvalid(id: Long): Boolean =
    val idString: String = id.toString
    val n: Int = idString.length
    if n % 2 != 0 then false
    else
      val (n1, n2): (String, String) = idString.splitAt(n / 2)
      n1 == n2

  def totalInvalidIDSum(idRanges: List[IdRange]): Long =
    def getInvalidIdSumInRange(idRange: IdRange): Long =
      (idRange.start to idRange.end).foldLeft(0L)((acc, id) => if isIdInvalid(id) then acc + id else acc)

    idRanges.foldLeft(0L)((acc, range) => acc + getInvalidIdSumInRange(range))

  def main(args: Array[String]): Unit =
    val reader: Iterator[String] = scala.io.Source.fromResource("input02.txt").getLines()
    val idRanges: List[IdRange] = parseRanges(reader.next())
    val result: Long = totalInvalidIDSum(idRanges)
    println(result)
