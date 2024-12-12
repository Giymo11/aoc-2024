
extension [T](ordering: Map[T, Seq[T]])
  def isIndirectConflict(a: T, other: T) =
    ordering(other).exists(another => ordering.isOrdered(another, a))
  def isOrdered(a: T, other: T): Boolean =
    !ordering(other).contains(a) && (ordering(a).contains(other) || !isIndirectConflict(a, other))
  def isSequenceValid(update: Seq[T]) = update.sliding(2).forall { case Seq(a, other) =>
    ordering.isOrdered(a, other)
  }

object Today extends AocDay(5) {
  def parseInput(input: String) =
    val Array(orderings, updateStrings) = input.split("\n\\s*\n").map(_.as_lines)
    def parseOrderings(line: String) = line.split("\\|").map(_.toInt)
    val ordering = orderings.map(parseOrderings).groupMap(_.head)(_(1)).withDefaultValue(Nil)
    val updates = updateStrings.map(_.split(",").toSeq.map(_.toInt))
    (ordering, updates)

  def middleElement[T](seq: Seq[T]) = seq(seq.length / 2)

  def part1: AocPart = input =>
    val (ordering, updates) = parseInput(input)
    updates.filter(ordering.isSequenceValid).map(middleElement).sum

  def part2: AocPart = input =>
    val (ordering, updates) = parseInput(input)
    val invalidUpdates = updates.filterNot(ordering.isSequenceValid)
    invalidUpdates.map(_.sortWith(ordering.isOrdered)).map(middleElement).sum
}

Today.solve()
