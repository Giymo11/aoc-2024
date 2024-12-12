
object Today extends AocDay(7) {
  type Operator = (Long, Long) => Long
  def isValid(goal: Long, cum: Long, todo: Seq[Long], ops: Seq[Operator]): Boolean = todo match
    case _ if cum > goal => false
    case Nil             => cum == goal
    case x :: others     => ops.exists(op => isValid(goal, op(cum, x), others, ops))
  extension (x: Seq[Long]) def checkWith(ops: Seq[Operator]) = isValid(x.head, x(1), x.drop(2), ops)

  def part1: AocPart = input =>
    val operators = Seq[Operator]((a, b) => a * b, (a, b) => a + b)
    input.as_lines.map(_.as_integers).filter(_.checkWith(operators)).map(_.head).sum

  def part2: AocPart = input =>
    def concat: Operator = (a, b) => (a.toString + b.toString).toLong
    val operators = Seq[Operator]((a, b) => a * b, (a, b) => a + b, concat)
    input.as_lines.map(_.as_integers).filter(_.checkWith(operators)).map(_.head).sum
}

Today.solve()
