//> using file ../aoclib
import scala.annotation.tailrec

type Region = Set[Coord]
extension (grid: Map[Coord, Char])
  def findRegions: Set[Region] = findRegionsFor(grid.keySet)

  @tailrec def findRegionsFor(region: Region, cum: Set[Region] = Set.empty): Set[Region] =
    if !region.isEmpty then
      val firstRegion = findRegionAt(region.head)
      findRegionsFor(region -- firstRegion, cum + firstRegion)
    else cum

  def findRegionAt(coord: Coord, found: Region = Set.empty): Region =
    def guard(candiate: Coord) = grid(candiate) == grid(coord) && !found.contains(candiate)
    Neighborhood.straight.with_offset(coord).filter(guard).toSet match
      case x if x.isEmpty => found + coord
      case some => some.foldLeft(found)((cum, coord) => cum | findRegionAt(coord, cum + coord))

extension (region: Region)
  def area = region.size
  def perimeter = region.sumBy(Neighborhood.straight.with_offset(_).count(!region.contains(_)))
  def sides = region.sumBy(x => Neighborhood.countCornersBy(y => region.contains(x + y)))

object Today extends AocDay(12) {
  def part1: AocPart = _.as_board.findRegions.toSeq.map(r => r.area * r.perimeter).sum
  def part2: AocPart = _.as_board.findRegions.toSeq.map(r => r.area * r.sides).sum
}

Today.solve()
