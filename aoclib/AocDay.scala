type AocPart = String => Any

import pprint._

trait AocDay(day: Int) {
  def part1: AocPart
  def part2: AocPart

  def benchmark[T](block: => T): (T, String) = {
    val start_time = System.nanoTime()
    val result = block
    val end_time = System.nanoTime()
    val diff_in_ms = (end_time - start_time) / 1e6
    (result, f" in $diff_in_ms%11f ms")
  }

  def memo[K, V](realFn: K => V): K => V = {
    val cache = collection.mutable.Map.empty[K, V]
    k => cache.getOrElseUpdate(k, realFn(k))
  }

  def solve() =
    val wd = os.pwd / "input"

    val test_input = os.read(wd / s"test$day.txt").replace("\r\n", "\n")
    pprintln(part1(test_input))
    pprintln(part2(test_input))

    val big_input = os.read(wd / s"input$day.txt").replace("\r\n", "\n")
    pprintln("part1: " + benchmark(part1(big_input)))
    pprintln("part2: " + benchmark(part2(big_input)))
}

extension (input: String) {
  def as_lines: Seq[String] = input.linesIterator.toSeq

  def as_integers: Seq[Long] =
    val integer_pattern = """\d+""".r
    integer_pattern.findAllIn(input).map(_.toLong).toSeq

  def as_grid: Seq[((Int, Int), Char)] = for {
    (line, row_index) <- input.as_lines.zipWithIndex
    (char, col_index) <- line.zipWithIndex
  } yield ((row_index, col_index), char)

  def as_board: Map[(Int, Int), Char] = as_grid.toMap.withDefaultValue('.')

  def renderVisited(visited: Set[Coord], mark: Char) =
    val (rows, cols) = (input.as_lines(0).size, input.as_lines.size)
    val marked = input.as_grid.map((coord, char) => if visited.contains(coord) then mark else char)
    marked.sliding(rows, cols).map(_.mkString).mkString("\n")
}

extension [T](seq: Iterable[T]) def sumBy(f: T => Int) = seq.foldLeft(0)(_ + f(_))
