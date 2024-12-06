import scala.annotation.tailrec

import $ivy.`org.scala-lang.modules::scala-parallel-collections:1.0.4`
import scala.collection.parallel.CollectionConverters._

import $file.lib.AocDay
import AocDay._

import $file.lib.Neighborhoods
import Neighborhoods._

type Visits = Set[(Coord, (Int, Int))]
extension (grid: Map[Coord, Char])
  @tailrec def walkFrom(position: Coord, direction: (Int, Int), visits: Visits): (Visits, Boolean) =
    val nextVisit = (position + direction, direction)
    if visits.contains(nextVisit) then (visits, true) // its a loop
    else if !grid.contains(nextVisit._1) then (visits, false) // not a loop
    else if grid(nextVisit._1) == '#' then walkFrom(position, direction.clockwise, visits)
    else walkFrom(nextVisit._1, direction, visits + nextVisit)
  def visitedCoordsFrom(start: Coord) = grid.walkFrom(start, Direction.up, Set())._1.map(_._1)
  def isLoopFrom(start: Coord) = grid.walkFrom(start, Direction.up, Set())._2

object Today extends AocDay(6) {
  def part1: AocPart = input =>
    val grid = input.as_grid.toMap
    val start = grid.find(_._2 == '^').map(_._1).get
    grid.visitedCoordsFrom(start).size + 1 // add start position

  def part2: AocPart = input =>
    val grid = input.as_grid.toMap
    val start = grid.find(_._2 == '^').map(_._1).get
    grid.visitedCoordsFrom(start).par.count(tile => (grid + (tile -> '#')).isLoopFrom(start))
}

@main
def main(): Unit = Today.solve()
