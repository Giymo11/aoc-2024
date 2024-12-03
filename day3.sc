import collection._

import $file.AocDay
import ammonite.$file.AocDay._

type Mul = Seq[Int]

implicit class MemoryOps(input: String) {
  def in_one_go = input.as_lines.mkString("")

  def parse_muls =
    val mul_pattern = """mul\((\d+),\s*(\d+)\)""".r
    import scala.util.matching.Regex.Match
    def matches_to_muls = (item: Match) => Seq(item.group(1), item.group(2)).map(_.toInt)
    mul_pattern.findAllMatchIn(input).map(matches_to_muls).toSeq

  def remove_disabled =
    // the .*? makes it "lazy", meaning it will stop at the shortest possible match instead of the longest
    val disabled_pattern = """don't\(\).*?do\(\)""".r
    disabled_pattern.replaceAllIn(input, "")
}

object Today extends AocDay(3) {
  def eval_muls = (input: Seq[Mul]) => input.map(_.product).sum
  def part1: AocPart = input => eval_muls(input.in_one_go.parse_muls).toString()
  def part2: AocPart = input => eval_muls(input.in_one_go.remove_disabled.parse_muls).toString()
}

@main
def main() = Today.solve()
