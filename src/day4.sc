import $file.AocDay
import ammonite.$file.AocDay._

import $file.Neighborhoods
import ammonite.$file.Neighborhoods._

object Today extends AocDay(4) {
  def part1: AocPart = input =>
    val grid = input.as_grid.toMap.withDefaultValue('.')
    val candidates = for {
      startCoord <- input.as_grid.collect { case (coord, char) if char == 'X' => coord }
      neighborhood <- Neighborhood.full.extend_to(1 to 3)
    } yield neighborhood.with_offset(startCoord).map(grid).mkString
    candidates.count(_ == "MAS").toString

  def part2: AocPart = input =>
    val grid = input.as_grid.toMap.withDefaultValue('.')
    val candidates = for {
      startCoord <- input.as_grid.collect { case (coord, char) if char == 'A' => coord }
    } yield Neighborhood.diagonal.with_offset(startCoord).map(grid).mkString
    candidates.count(Seq("MSMS", "SMSM", "MMSS", "SSMM").contains).toString
}

@main
def main(): Unit = Today.solve()