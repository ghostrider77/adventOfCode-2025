import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AoCSuite extends AnyFreeSpec, Matchers {
  "Day 01" - {
    val inputLines: List[String] = List("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")
    val startPosition: Int = 50

    "Part 1" - {
      import Day01a.{Rotation, countZeroPositions, parseInput}

      "should count the number of times the dial is left pointing at 0 after any rotation in the sequence" in {
        val rotations: List[Rotation] = inputLines.map(parseInput)
        countZeroPositions(startPosition, rotations) shouldEqual 3
      }
    }

    "Part 2" - {
      import Day01b.{Rotation, countTotalZeroPositionPasses, parseInput}

      "should count the total number of times the dial passes 0 after any rotation in the sequence" in {
        val rotations: List[Rotation] = inputLines.map(parseInput)
        countTotalZeroPositionPasses(startPosition, rotations) shouldEqual 6
      }
    }
  }

  "Day02" - {
    "Part 1" - {
      import Day02a.{IdRange, totalInvalidIDSum}

      "should add all of the invalid IDs" - {
        val ranges: List[IdRange] = List(IdRange(11, 22), IdRange(95, 115), IdRange(1188511880L, 1188511890L))
        totalInvalidIDSum(ranges) shouldEqual 1188512017L
      }
    }

    "Part 2" - {
      import Day02b.{IdRange, totalInvalidIDSum}

      "should add all of the invalid IDs" - {
        val ranges: List[IdRange] = List(IdRange(11, 22), IdRange(95, 115), IdRange(1188511880L, 1188511890L))
        totalInvalidIDSum(ranges) shouldEqual 1188512128L
      }
    }
  }

  "Day03" - {
    val packs: List[List[Int]] = List(
      List(9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1),
      List(8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9),
      List(2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8),
      List(8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1)
    )

    "Part 1" - {
      import Day03a.calcSumOfJoltages

      "should calculate the total output joltage when 2 batteries are turned on" in {
        calcSumOfJoltages(packs) shouldEqual 357
      }
    }

    "Part 2" - {
      import Day03b.calcSumOfJoltages

      "should calculate the total output joltage when 12 batteries are turned on" in {
        calcSumOfJoltages(packs, 12) shouldEqual 3121910778619L
      }
    }
  }

  "Day04" - {
    val cells: List[String] = List(
      "..@@.@@@@.",
      "@@@.@.@.@@",
      "@@@@@.@.@@",
      "@.@@@@..@.",
      "@@.@@@@.@@",
      ".@@@@@@@.@",
      ".@.@.@.@@@",
      "@.@@@.@@@@",
      ".@@@@@@@@.",
      "@.@.@@@.@."
    )

    "Part 1" - {
      import Day04a.{Department, parseInput, countAccessableRolls}

      "should calculate the number of rolls of paper that can be accessed by a forklift" in {
        val data: Department = parseInput(cells)
        countAccessableRolls(data) shouldEqual 13
      }
    }

    "Part 2" - {
      import Day04b.{Department, parseInput, countRemovableRolls}

      "should calculate the number of rolls of paper that can be removed by a forklift iteratively" in {
        val data: Department = parseInput(cells)
        countRemovableRolls(data) shouldEqual 43
      }
    }
  }

  "Day05" - {
    "Part 1" - {
      import Day05a.{IDRange, getNrOfFreshIDs}

      "should calculate the number of fresh ingredient IDs" in {
        val ranges: List[IDRange] = List(IDRange(3, 5), IDRange(10, 14), IDRange(16, 20), IDRange(12, 18))
        val availableIDs: List[Long] = List(1, 5, 8, 11, 17, 32)
        getNrOfFreshIDs(ranges, availableIDs) shouldEqual 3L
      }
    }

    "Part 2" - {
      import Day05b.{IDRange, getNrOfAllFreshIDs}

      "should calculate the number of all possible fresh ingredient IDs" in {
        val ranges: List[IDRange] = List(IDRange(3, 5), IDRange(10, 14), IDRange(16, 20), IDRange(12, 18))
        getNrOfAllFreshIDs(ranges) shouldEqual 14L
      }
    }
  }

  "Day06" - {
    import Day06a.{MathProblem, Operation, calcGrandTotal}

    "should calculate the grand total found by adding together all of the answers to the individual problems" in {
      val problems: List[MathProblem] = List(
        MathProblem(List(123, 45, 6), Operation.Mul),
        MathProblem(List(328, 64, 98), Operation.Add),
        MathProblem(List(51, 387, 215), Operation.Mul),
        MathProblem(List(64, 23, 314), Operation.Add)
      )
      calcGrandTotal(problems) shouldEqual 4277556L
    }
  }

  "Day07" - {
    val splitterCoords: Set[(Int, Int)] = Set(
      (2, 7),
      (4, 6),
      (4, 8),
      (6, 5),
      (6, 7),
      (6, 9),
      (8, 4),
      (8, 6),
      (8, 10),
      (10, 3),
      (10, 5),
      (10, 9),
      (10, 11),
      (12, 2),
      (12, 6),
      (12, 12),
      (14, 1),
      (14, 3),
      (14, 5),
      (14, 7),
      (14, 9),
      (14, 13)
    )

    "Part 1" - {
      import Day07a.{Coord, Manifold, getNrOfsplits}

      "should calculate the number of times the beam is split" in {
        val splitters: Set[Coord] = splitterCoords.map(Coord(_, _))
        val manifold = Manifold(start = Coord(0, 7), splitters = splitters, nrRows = 16, nrCols = 15)
        getNrOfsplits(manifold) shouldEqual 21
      }
    }

    "Part 2" - {
      import Day07b.{Coord, Manifold, getNrOfParticlePaths}

      "should calculate the number of all paths that a particle can traverse" in {
        val splitters: Set[Coord] = splitterCoords.map(Coord(_, _))
        val manifold = Manifold(start = Coord(0, 7), splitters = splitters, nrRows = 16, nrCols = 15)
        getNrOfParticlePaths(manifold) shouldEqual 40L
      }
    }
  }
}
