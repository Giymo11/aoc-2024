import $file.lib.AocDay
import AocDay._

object Today extends AocDay(7) {
  type Operator = (Long, Long) => Long
  def isValid(goal: Long, cum: Long, todo: Seq[Long], ops: Seq[Operator]): Boolean = todo match
    case _ if cum > goal => false
    case Nil             => cum == goal
    case x :: others     => ops.exists(op => isValid(goal, op(cum, x), others, ops))

  def part1: AocPart = input =>
    val inputs = input.as_lines.map(_.as_integers)
    val operators = Seq[Operator]((a, b) => a * b, (a, b) => a + b)
    inputs.filter(line => isValid(line.head, line(1), line.drop(2), operators)).map(_.head).sum

  def part2: AocPart = input =>
    val inputs = input.as_lines.map(_.as_integers)
    def concat: Operator = (a, b) => (a.toString + b.toString).toLong
    val operators = Seq[Operator]((a, b) => a * b, (a, b) => a + b, concat)
    inputs.filter(line => isValid(line.head, line(1), line.drop(2), operators)).map(_.head).sum
}

@main def main(): Unit = Today.solve()
