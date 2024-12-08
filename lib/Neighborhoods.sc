type Coord = (Int, Int)
type Neighborhood = Seq[Coord]

extension (coord: Coord) {
  def +(other: Coord): Coord = (coord._1 + other._1, coord._2 + other._2)
  def *(other: Int): Coord = (coord._1 * other, coord._2 * other)
  def unary_- : Coord = (-coord._1, -coord._2)
  def -(other: Coord): Coord = coord + -other
  def <(other: Coord): Boolean = coord._1 < other._1 && coord._2 < other._2
  def clockwise: Coord = (coord._2, -coord._1)
}

object Direction {
  val up = (-1, 0)
  val down = (1, 0)
  val left = (0, -1)
  val right = (0, 1)
}

object Neighborhood {
  val straight = Seq(Direction.up, Direction.down, Direction.left, Direction.right)
  val diagonal = Seq((-1, -1), (-1, 1), (1, -1), (1, 1))
  val full = straight ++ diagonal
}

implicit class NeighborhoodOps(neighborhood: Neighborhood) {
  def extend_to(distances: Seq[Int]) = neighborhood.map(coord => distances.map(coord * _))
  def with_offset(offset: Coord) = neighborhood.map(offset + _)
}
