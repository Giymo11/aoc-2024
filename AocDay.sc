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

implicit class AocParser(input: String) {
  def as_lines: Seq[String] = input.linesIterator.toSeq
  def as_integers: Seq[Int] = 
    val integer_pattern = """\d+""".r
    integer_pattern.findAllIn(input).map(_.toInt).toSeq
}
