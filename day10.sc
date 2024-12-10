import $file.lib.AocDay
import AocDay._

import $file.lib.Neighborhoods
import Neighborhoods._

object Today extends AocDay(10) {
  def parseHikingMap(input: String): (Map[Coord, Char], Seq[Coord]) =
    (input.as_grid.toMap.withDefaultValue('.'), input.as_grid.filter(_._2 == '0').map(_._1))

  extension (grid: Map[Coord, Char])
    def getHikingTrailsFrom(current: Coord, cum: Seq[Coord] = Seq.empty): Set[Seq[Coord]] =
      if grid(current) == '9' then return Set(cum :+ current)
      val nexts = Neighborhood.straight.with_offset(current).filter(grid(_) == grid(current) + 1)
      nexts.map(next => getHikingTrailsFrom(next, cum :+ current)).fold(Set.empty)(_ | _)

  def part1: AocPart = input =>
    val (grid, startCoords) = parseHikingMap(input)
    startCoords.map(grid.getHikingTrailsFrom(_).map(_.last)).map(_.size).sum

  def part2: AocPart = input =>
    val (grid, startCoords) = parseHikingMap(input)
    startCoords.map(grid.getHikingTrailsFrom(_)).map(_.size).sum
}

@main def main(): Unit = Today.solve()
