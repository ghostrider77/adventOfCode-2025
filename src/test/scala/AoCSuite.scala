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
}
