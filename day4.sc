import $file.AocDay
import ammonite.$file.AocDay._

object Today extends AocDay(4) {
  def part1: AocPart = input =>
    val grid = input.as_grid.toMap.withDefaultValue('.')
    val starting_points = input.as_grid.collect { case (coord, char) if char == 'X' => coord }

    val directions = Neighborhood.full.withRange(1 to 3)
    def getCandidates(start: Coord): Seq[String] = for {
      direction <- directions
      coords = direction.map(_ + start)
      chars = coords.map(grid)
    } yield chars.mkString

    starting_points.flatMap(getCandidates).count(_ == "MAS").toString()

  def part2: AocPart = input =>
    val grid = input.as_grid.toMap.withDefaultValue(' ')
    val starting_points = input.as_grid.collect { case (coord, char) if char == 'A' => coord }

    def getCrossShape(start: Coord) = Neighborhood.diagonal.map(_ + start).map(grid).mkString
    val valid = Seq("MSMS", "SMSM", "MMSS", "SSMM")

    starting_points.map(getCrossShape).count(valid.contains).toString()
}

@main
def main(): Unit = Today.solve()
