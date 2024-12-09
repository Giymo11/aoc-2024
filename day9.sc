import $file.lib.AocDay
import AocDay._

import scala.annotation.tailrec

object Today extends AocDay(9) {
  type IdRanges = Seq[(Int, Int, Int)]
  extension (numbers: Seq[Int])
    def toRanges(index: Int, id: Int, cum: IdRanges, isEmpty: Boolean): IdRanges = numbers match {
      case Nil => cum
      case head +: tail =>
        if (head == 0) tail.toRanges(index, id, cum, !isEmpty)
        else {
          val until = index + head
          if (isEmpty) tail.toRanges(until, id, cum :+ (index, until - 1, -1), !isEmpty)
          else tail.toRanges(until, id + 1, cum :+ (index, until - 1, id), !isEmpty)
        }
    }

  def moveBlocks(dest: IdRanges, src: IdRanges, cum: IdRanges): IdRanges = (dest, src) match
    case (Nil, _) => cum
    case (_, Nil) => cum
    case ((destFrom, destTo, destId) +: destTail, (srcFrom, srcTo, srcId) +: srcTail) =>
      if destFrom >= srcFrom then cum
      else if destId != -1 && destTo < srcFrom then
        moveBlocks(destTail, src, cum :+ (destFrom, destTo, destId))
      else if srcId == -1 then moveBlocks(dest, srcTail, cum)
      else
        val (srcSize, destSize) = (srcTo - srcFrom + 1, destTo - destFrom + 1)
        val step = Math.min(destSize, srcSize)
        val updatedDest =
          if (destSize > step) (destFrom + step, destTo, destId) +: destTail else destTail
        val updatedSrc = if (srcSize > step) (srcFrom + step, srcTo, srcId) +: srcTail else srcTail
        moveBlocks(updatedDest, updatedSrc, cum :+ (destFrom, destFrom + step - 1, srcId))

  

  def checksum(from: Int, to: Int, id: Int): Long =
    (from to to).map(_ * id.toLong).sum

  def render(compacted: IdRanges) =
    compacted.flatMap((from, to, id) => (from to to).map(_ => id)).mkString

  def part1: AocPart = input =>
    val numbers = input.split("").map(_.toInt)

    val ranges = numbers.toSeq.toRanges(0, 0, Seq.empty, false)
    val compacted = moveBlocks(ranges, ranges.reverse, Seq.empty)
    compacted.map(checksum).sum

  def part2: AocPart = input => ""
}

@main def main(): Unit = Today.solve()
