object Day02b:
  case class IdRange(start: Long, end: Long)

  private def parseRanges(line: String): List[IdRange] =
    def parse(item: String): IdRange = item match
      case s"$start-$end" => IdRange(start.toLong, end.toLong)
      case _ => throw Exception(s"Invalid range $item.")

    line.split(",").map(parse).toList

  private def isIdInvalid(id: Long): Boolean =
    val idString: String = id.toString
    def isConsistOfRepeatedSubstring(k: Int): Boolean =
      val (h, tl): (String, String) = idString.splitAt(k)
      if tl.length % k != 0 then false
      else tl.grouped(k).forall(_ == h)

    val maxPrefixSize: Int = idString.length / 2
    (1 to maxPrefixSize).exists(isConsistOfRepeatedSubstring)

  def totalInvalidIDSum(idRanges: List[IdRange]): Long =
    def getInvalidIdSumInRange(idRange: IdRange): Long =
      (idRange.start to idRange.end).foldLeft(0L)((acc, id) => if isIdInvalid(id) then acc + id else acc)

    idRanges.foldLeft(0L)((acc, range) => acc + getInvalidIdSumInRange(range))

  def main(args: Array[String]): Unit =
    val reader: Iterator[String] = scala.io.Source.fromResource("input02.txt").getLines()
    val idRanges: List[IdRange] = parseRanges(reader.next())
    val result: Long = totalInvalidIDSum(idRanges)
    println(result)
