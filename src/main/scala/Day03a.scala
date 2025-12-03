object Day03a:
  type Pack = List[Int]

  private def parseInput(line: String): Pack =
    line.toList.map(_.asDigit)

  def calcSumOfJoltages(packs: List[Pack]): Int =
    def getMaxJoltage(pack: Pack): Int =
      val n: Int = pack.length
      val (a, ix): (Int, Int) = pack.zipWithIndex.maxBy { case (joltage, _) => joltage }
      if ix == n - 1 then
        val b: Int = pack.init.max
        10 * b + a
      else
        val b: Int = pack.drop(ix + 1).max
        10 * a + b

    packs.foldLeft(0)((acc, pack) => acc + getMaxJoltage(pack))

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input03.txt").getLines()
    val batteryPacks: List[Pack] = lines.map(parseInput).toList
    val result: Int = calcSumOfJoltages(batteryPacks)
    println(result)
