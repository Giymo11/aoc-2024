
type AocPart = String => Any

trait AocDay(day: Int) {
  def part1: AocPart
  def part2: AocPart

  def solve() =
    val wd = os.pwd / "input"

    val test_input = os.read(wd / s"test$day.txt")
    println(part1(test_input))
    println(part2(test_input))

    val input1 = os.read(wd / s"input$day.txt")
    println("part1: " + part1(input1))
    println("part2: " + part2(input1))
}

implicit class AocParser(input: String) {
  def as_lines: Seq[String] = input.linesIterator.toSeq

  def as_integers: Seq[Int] =
    val integer_pattern = """\d+""".r
    integer_pattern.findAllIn(input).map(_.toInt).toSeq

  def as_grid: Seq[((Int, Int), Char)] = for {
    (line, row_index) <- input.as_lines.zipWithIndex
    (char, col_index) <- line.zipWithIndex
  } yield ((row_index, col_index), char)
}