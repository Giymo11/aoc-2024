import $file.lib.AocDay
import AocDay._

import $file.lib.Neighborhoods
import Neighborhoods._

object Today extends AocDay(8) {
  def part1: AocPart = input =>
    val grid = input.as_grid.toMap
    val antennas = grid.filterNot(_._2 == '.').groupMap(_._2)(_._1)
    def findAntinodes(antennas: Seq[Coord]): Seq[Coord] =
      val slope = antennas(0) - antennas(1)
      Seq(antennas(0) + slope, antennas(1) - slope).filter(grid.contains)
    antennas.values.map(_.toSeq.combinations(2).flatMap(findAntinodes).toSet).reduce(_ | _).size

  def part2: AocPart = input =>
    val grid = input.as_grid.toMap
    val antennas = grid.filterNot(_._2 == '.').groupMap(_._2)(_._1)
    def findAllAntinodes(antennas: Seq[Coord]): Seq[Coord] =
      val slope = antennas(0) - antennas(1)
      val left = LazyList.iterate(antennas(0))(_ + slope).takeWhile(grid.contains)
      val right = LazyList.iterate(antennas(1))(_ - slope).takeWhile(grid.contains)
      left ++ right
    antennas.values.map(_.toSeq.combinations(2).flatMap(findAllAntinodes).toSet).reduce(_ | _).size
}

@main def main(): Unit = Today.solve()
