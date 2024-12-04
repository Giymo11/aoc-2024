type Coord = (Int, Int)
type Neighborhood = Seq[Coord]

implicit class CoordOps(coord: Coord) {
  def +(other: Coord): Coord = (coord._1 + other._1, coord._2 + other._2)
  def *(other: Int): Coord = (coord._1 * other, coord._2 * other)
}

object Neighborhood {
  val straight = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
  val diagonal = Seq((-1, -1), (-1, 1), (1, -1), (1, 1))
  val full = straight ++ diagonal
}

implicit class NeighborhoodOps(neighborhood: Neighborhood) {
  def extend_to(distances: Seq[Int]) = neighborhood.map(coord => distances.map(coord * _))
  def with_offset(offset: Coord) = neighborhood.map(offset + _)
}
