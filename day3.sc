import collection._

import $file.AocDay
import ammonite.$file.AocDay._

type Mul = Seq[Int]
implicit class MemoryOps(input: String) {
  def parse_muls =
    val mul_pattern = """(?sx)  # Enable verbose mode, make `.` match newlines
    mul\(       # Match `mul(`
      (\d+)     # Capture first integer
    ,        # Match `,`
      (\d+)     # Capture second integer
    \)          # Match `)` """.r
    import scala.util.matching.Regex.Match
    def matches_to_muls = (item: Match) => Seq(item.group(1), item.group(2)).map(_.toInt)
    mul_pattern.findAllMatchIn(input).map(matches_to_muls).toSeq

  def remove_disabled =
    val disabled_pattern = """(?sx) # Enable verbose and dot-all mode
    don't\(\)   # Match `don't()`       
      .*?       # Match anything
    do\(\)      # Match `do()`  """.r
    disabled_pattern.replaceAllIn(input, "")
}

object Today extends AocDay(3) {
  def eval_muls = (input: Seq[Mul]) => input.map(_.product).sum
  def part1: AocPart = input => eval_muls(input.parse_muls).toString()
  def part2: AocPart = input => eval_muls(input.remove_disabled.parse_muls).toString()
}

@main
def main() = Today.solve()
