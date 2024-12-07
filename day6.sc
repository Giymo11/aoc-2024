import scala.annotation.tailrec

import $ivy.`org.scala-lang.modules::scala-parallel-collections:1.0.4`
import scala.collection.parallel.CollectionConverters._

import $file.lib.AocDay
import AocDay._

import $file.lib.Neighborhoods
import Neighborhoods._

type Visit = (Coord, (Int, Int))
extension (visit: Visit)
  def next = (visit._1 + visit._2, visit._2)
  def coords = visit._1

type Visits = Set[Visit]
extension (grid: Map[Coord, Char])
  def step(current: Visit) = current.next match
    case (coords, _) if !grid.contains(coords)      => None
    case (coords, direction) if grid(coords) == '#' => Some(current.coords, direction.clockwise)
    case nextVisit                                  => Some(nextVisit)

  @tailrec def walk(current: Visit, visited: Set[Coord]): Set[Coord] = step(current) match
    case Some(nextVisit) => walk(nextVisit, visited + nextVisit.coords)
    case None            => visited

  @tailrec def isLooping(current: Visit, visited: Visits = Set()): Boolean = step(current) match
    case Some(nextVisit) if visited.contains(nextVisit) => true
    case Some(nextVisit)                                => isLooping(nextVisit, visited + nextVisit)
    case None                                           => false

object Today extends AocDay(6) {
  def render(input: String, visited: Set[Coord]) =
    val (rows, cols) = (input.as_lines(0).size, input.as_lines.size)
    val marked = input.as_grid.map((coord, char) => if visited.contains(coord) then 'X' else char)
    marked.sliding(rows, cols).map(_.mkString).mkString("\n")

  def part1: AocPart = input =>
    val grid = input.as_grid.toMap
    val startPosition = grid.find(_._2 == '^').map(_._1).get
    grid.walk((startPosition, Direction.up), Set(startPosition)).size

  def part2: AocPart = input =>
    val grid = input.as_grid.toMap
    val start = (grid.find(_._2 == '^').map(_._1).get, Direction.up)
    grid.walk(start, Set.empty).par.count(tile => (grid + (tile -> '#')).isLooping(start))
}

@main
def main(): Unit = Today.solve()
