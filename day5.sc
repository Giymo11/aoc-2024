import $file.AocDay
import ammonite.$file.AocDay._

type Ordering[T] = Map[T, Seq[T]]
implicit class OrderingOps[T](ordering: Ordering[T]) {
  def isOrderedIndirectly(a: T, other: T) =
    !ordering(other).exists(another => ordering.isOrdered(another, a))
  def isOrdered(a: T, other: T): Boolean =
    !ordering(other).contains(a) &&
      (!ordering.contains(other) || ordering(a).contains(other) ||
        ordering.isOrderedIndirectly(a, other))
  def isUpdateValid(update: Seq[T]) = update.sliding(2).forall { case Seq(a, other) =>
    ordering.isOrdered(a, other)
  }
}

object Today extends AocDay(5) {
  def parseInts(line: String) = line.split("\\|").map(_.toInt)
  def parseInput(input: String) =
    val Array(orderings, updates) = input.split("\n\\s*\n").map(_.as_lines)
    val ordering = orderings.map(parseInts).groupMap(_.head)(_(1)).withDefaultValue(Nil)
    val updateLists = updates.map(_.split(",").map(_.toInt).toSeq)
    (ordering, updateLists)

  def middleElement[T](seq: Seq[T]) = seq(seq.length / 2)

  def part1: AocPart = input =>
    val (ordering, updates) = parseInput(input)
    updates.filter(ordering.isUpdateValid).map(middleElement).sum.toString

  def part2: AocPart = input =>
    val (ordering, updates) = parseInput(input)
    val invalidUpdates = updates.filterNot(ordering.isUpdateValid)
    invalidUpdates.map(_.sortWith(ordering.isOrdered)).map(middleElement).sum.toString
}

@main
def main(): Unit = Today.solve()
