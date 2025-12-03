object Day03b:
  import scala.annotation.tailrec
  import scala.math.Ordered.orderingToOrdered

  type Pack = List[Int]

  given packOrdering: Ordering[Pack] with
    override def compare(p1: Pack, p2: Pack): Int =
      p1.mkString.toLong.compareTo(p2.mkString.toLong)

  private def parseInput(line: String): Pack =
    line.toList.map(_.asDigit)

  private def getMaxJoltage(pack: Pack, size: Int): Long =
    val (initial, rest): (Pack, Pack) = pack.reverse.splitAt(size)

    @tailrec
    def loop(acc: Pack, items: Pack): Long = items match
      case Nil => acc.mkString.toLong
      case x :: xs =>
        val candidate: Pack =
          (0 until size).map(ix => x :: acc.zipWithIndex.collect { case (k, i) if i != ix => k }).max
        if acc > candidate then loop(acc, xs)
        else loop(candidate, xs)

    loop(initial.reverse, rest)

  def calcSumOfJoltages(packs: List[Pack], size: Int): Long =
    packs.foldLeft(0L)((acc, pack) => acc + getMaxJoltage(pack, size))

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input03.txt").getLines()
    val size: Int = 12
    val batteryPacks: List[Pack] = lines.map(parseInput).toList
    val result: Long = calcSumOfJoltages(batteryPacks, size)
    println(result)
