import scala.annotation.tailrec

object Day10a:
  import scala.collection.mutable.Queue as MutableQueue

  enum State:
    case On, Off

    def unary_- : State = this match
      case On => Off
      case Off => On

  object State:
    def apply(c: Char): State = c match
      case '.' => Off
      case '#' => On
      case _ => throw Exception(s"Unknown state $c.")

  case class MachineConfig(targetLights: List[State], buttons: List[Set[Int]]):
    val initialLights: List[State] = targetLights.map(_ => State.Off)

    def pressButtons(indicatorLights: List[State]): List[List[State]] =
      buttons
        .map(bs => indicatorLights.zipWithIndex.map { case (state, ix) => if bs.contains(ix) then -state else state })

  private def parseInput(lines: List[String]): List[MachineConfig] =
    def parseLine(line: String): MachineConfig = line match
      case s"[$ls] $bs {$_}" =>
        val targetLights: List[State] = ls.toList.map(State(_))
        val buttons = bs.split(" ").map(_.drop(1).dropRight(1).split(",").map(_.toInt).toSet).toList
        MachineConfig(targetLights, buttons)
      case _ => throw Exception(s"Malformed input $line")

    lines.map(parseLine)

  private def findShortestPath(machineConfig: MachineConfig): Int =
    val queue: MutableQueue[(List[State], Int)] = MutableQueue.empty[(List[State], Int)]
    queue.enqueue((machineConfig.initialLights, 0))

    @tailrec
    def loop(visitedStates: Set[List[State]]): Int =
      if queue.isEmpty then -1
      else
        val (indicatorLights, distance): (List[State], Int) = queue.dequeue()
        if indicatorLights == machineConfig.targetLights then distance
        else
          val nextLights: List[List[State]] =
            machineConfig.pressButtons(indicatorLights).filter(nl => !visitedStates.contains(nl))
          queue.enqueueAll(nextLights.map((_, distance + 1)))
          loop(visitedStates ++ nextLights)

    loop(Set(machineConfig.initialLights))

  def calcSumOfshortestPaths(machineConfigs: List[MachineConfig]): Int =
    machineConfigs.foldLeft(0)((acc, config) => acc + findShortestPath(config))

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input10.txt").getLines()
    val machines: List[MachineConfig] = parseInput(lines.toList)
    val result: Int = calcSumOfshortestPaths(machines)
    println(result)
