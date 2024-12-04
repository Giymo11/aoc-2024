type AocPart = String => String

trait AocDay(day: Int) {
  def part1: AocPart
  def part2: AocPart

  def solve() =
    val wd = os.pwd / "aoc-input"

    val test_input = os.read(wd / s"test$day.txt")
    println(part1(test_input))
    println(part2(test_input))

    val input1 = os.read(wd / s"input$day.txt")
    println("part1: " + part1(input1))
    println("part2: " + part2(input1))
}

type Coord = (Int, Int)
type Neighborhood = Seq[Coord]

implicit class AocParser(input: String) {
  def as_lines: Seq[String] = input.linesIterator.toSeq

  def as_integers: Seq[Int] =
    val integer_pattern = """\d+""".r
    integer_pattern.findAllIn(input).map(_.toInt).toSeq

  def as_grid: Seq[(Coord, Char)] = for {
    (line, row_index) <- input.as_lines.zipWithIndex
    (char, col_index) <- line.zipWithIndex
  } yield ((row_index, col_index), char)
}

implicit class WordsearchOps(input: String) {
  def rotate45: String =
    val indexedChars = for {
      (line, rowIndex) <- input.as_lines.zipWithIndex
      (chara, columnIndex) <- line.zipWithIndex
    } yield (rowIndex + columnIndex, chara)
    val indexedStrings = indexedChars.groupMapReduce(_._1)(_._2.toString)(_ + _)
    indexedStrings.toSeq.sortBy(_._1).map(_._2).mkString("\n")

  def rotate90: String = input.as_lines.transpose.flatMap(_.mkString + "\n").mkString
}

implicit class CoordOps(coord: Coord) {
  def +(other: Coord): Coord = (coord._1 + other._1, coord._2 + other._2)
}

object Neighborhood {
  val straight = Seq((-1, 0), (1, 0), (0, -1), (0, 1))
  val diagonal = Seq((-1, -1), (-1, 1), (1, -1), (1, 1))
  val full = straight ++ diagonal
}

implicit class NeighborhoodOps(neighborhood: Neighborhood) {
  def withRange(distances: Seq[Int]): Seq[Seq[Coord]] =
    neighborhood.map((x, y) => distances.map(dist => (x * dist, y * dist)))
}
