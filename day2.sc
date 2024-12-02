import collection._

import $file.AocDay
import ammonite.$file.AocDay._

enum Sign {
  case Increasing
  case Decreasing
  case Neutral
}

type Report = Seq[Int]
implicit class ReportOps(report: Report) {
  def without(index: Int): Report = report.patch(index, Seq.empty, 1)

  def is_safe_with_tolerance(tolerance: Int): Boolean =
    def get_unsafe_indices(signum: Sign) = for
      (pair, index) <- report.sliding(2).zipWithIndex.toSeq
      if !pair.is_safe(signum)
    yield index

    val unsafe_indices = Seq(Sign.Increasing, Sign.Decreasing).map(get_unsafe_indices)

    if unsafe_indices.exists(_.isEmpty) then true
    else if tolerance > 0 then
      val shortest_unsafe_indices = unsafe_indices.minBy(_.size)
      val indices_to_try          = shortest_unsafe_indices :+ shortest_unsafe_indices.last + 1
      indices_to_try.exists(index => report.without(index).is_safe_with_tolerance(tolerance - 1))
    else false
}

type Pairwise = Seq[Int]
implicit class PairwiseOps(pair: Pairwise) {
  def is_safe(valid_sign: Sign): Boolean = pair match
    case Seq(a, b) => is_difference_ok(a, b) && signum(a, b) == valid_sign
    case _         => false

  val is_difference_ok = (a: Int, b: Int) =>
    val diff = (b - a).abs
    diff >= 1 && diff <= 3

  val signum = (a: Int, b: Int) =>
    if a > b then Sign.Decreasing else if a < b then Sign.Increasing else Sign.Neutral
}

object Today extends AocDay(2) {
  def parse_reports(input: String): Seq[Report] =
    input.as_lines.map(_.as_integers)

  def part1: AocPart = input =>
    val reports = parse_reports(input)
    reports.count(_.is_safe_with_tolerance(0)).toString

  def part2: AocPart = input =>
    val reports = parse_reports(input)
    reports.count(_.is_safe_with_tolerance(1)).toString
}

@main
def main() = Today.solve()