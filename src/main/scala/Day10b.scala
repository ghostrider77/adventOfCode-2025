import optimus.algebra._
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPIntVar

object Day10b:
  case class MachineConfig(buttons: List[Set[Int]], targetLevels: List[Int])

  private def parseInput(lines: List[String]): List[MachineConfig] =
    def parseLine(line: String): MachineConfig = line match
      case s"[$_] $bs {$jl}" =>
        val targetLevels: List[Int] = jl.split(",").map(_.toInt).toList
        val buttons = bs.split(" ").map(_.drop(1).dropRight(1).split(",").map(_.toInt).toSet).toList
        MachineConfig(buttons, targetLevels)
      case _ => throw Exception(s"Malformed input $line")

    lines.map(parseLine)

  private def calcShortestPath(machineConfig: MachineConfig): Long =
    val MachineConfig(buttons, targetLevels) = machineConfig
    val n: Int = targetLevels.length
    val m: Int = buttons.length

    given problem: MPModel = MPModel(SolverLib.oJSolver)

    val matrix: Array[Array[Int]] =
      (0 until n).map(i => buttons.map(bs => if bs.contains(i) then 1 else 0).toArray).toArray
    val b: Array[Int] = targetLevels.toArray
    val x: Array[MPIntVar] = Array.tabulate(m)(j => MPIntVar(s"x$j", 0 to b.max))

    for i <- 0 until n do
      var expr: Expression = 0
      for (j <- 0 until m) do
        if (matrix(i)(j) == 1) expr += x(j)

      add(expr := b(i))

    minimize(x.reduce(_ + _))
    start()
    val result: Long = math.round(objectiveValue)
    release()
    result

  def calcSumOfshortestPaths(machineConfigs: List[MachineConfig]): Long =
    machineConfigs.foldLeft(0L)((acc, config) => acc + calcShortestPath(config))

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input10.txt").getLines()
    val machines: List[MachineConfig] = parseInput(lines.toList)
    val result: Long = calcSumOfshortestPaths(machines)
    println(result)
