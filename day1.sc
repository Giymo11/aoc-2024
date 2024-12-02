// to run this script: amm -watch day-1.sc

import $file.AocDay
import ammonite.$file.AocDay._

object Today extends AocDay(1) {

  def part2: AocPart = input =>
    val (left, right) = parse_into_two_lists(input)

    val occurance_list = right.groupBy(identity).view.mapValues(_.size)
    val occurances = occurance_list.toMap.withDefaultValue(0)

    left.map(item => item * occurances(item)).sum.toString()

  def part1: AocPart = input =>
    val (left, right) = parse_into_two_lists(input)
    val pairwise = left.sorted.zip(right.sorted)
    
    def calc_distance = (a: Int, b: Int) => Math.abs(a - b)
    pairwise.map(calc_distance(_, _)).sum.toString()

  import scala.util.matching.Regex
  def parse_into_two_lists(input: String): (Seq[Int], Seq[Int]) =
    input.as_lines
      .map(_.as_integers)
      .collect { case Seq(a, b) => (a, b) }
      .unzip
}

@main
def main(): Unit = Today.solve()
