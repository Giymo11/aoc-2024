//> using file ../aoclib

import scala.collection.parallel.CollectionConverters._

object Today extends AocDay(11):
  def digits(num: Long) = num.toString.length
  val numStones: ((Long, Int)) => Long = memo:
    case (_, 0)      => 1
    case (0, blinks) => numStones(1, blinks - 1)
    case (stone, blinks) if digits(stone) % 2 == 0 =>
      stone.toString.grouped(digits(stone) / 2).map(half => numStones(half.toLong, blinks - 1)).sum
    case (stone, blinks) => numStones(stone * 2024, blinks - 1)

  def part1: AocPart = input => input.as_integers.par.map(numStones(_, 25)).sum

  def part2: AocPart = input => input.as_integers.par.map(numStones(_, 75)).sum
end Today

Today.solve()
