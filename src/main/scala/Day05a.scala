object Day05a:
  case class IDRange(start: Long, end: Long):
    def contains(k: Long): Boolean =
      start <= k && k <= end

  private def parseInput(lines: List[String]): (List[IDRange], List[Long]) =
    def parseRange(line: String): IDRange = line match
      case s"$a-$b" => IDRange(a.toLong, b.toLong)
      case _ => throw Exception(s"Malformed input $line.")

    val (ids, rest): (List[String], List[String]) = lines.span(_ != "")
    (ids.map(parseRange), rest.drop(1).map(_.toLong))

  def getNrOfFreshIDs(idRanges: List[IDRange], ingredientIDS: List[Long]): Int =
    ingredientIDS.count(id => idRanges.exists(_.contains(id)))

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input05.txt").getLines()
    val (idRanges, ingredientIDs): (List[IDRange], List[Long]) = parseInput(lines.toList)
    val result: Int = getNrOfFreshIDs(idRanges, ingredientIDs)
    println(result)
