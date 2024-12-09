import $file.lib.AocDay
import AocDay._

import scala.annotation.tailrec

object Today extends AocDay(9) {
  type IdRanges = Seq[(Int, Int, Int)]
  extension (numbers: Seq[Int])
    def toRanges(index: Int, id: Int, cum: IdRanges, isEmpty: Boolean): IdRanges = numbers match
      case Nil                       => cum
      case head +: tail if head == 0 => tail.toRanges(index, id, cum, !isEmpty)
      case head +: tail =>
        val nextIndex = index + head
        if isEmpty then tail.toRanges(nextIndex, id, cum :+ (index, nextIndex - 1, -1), !isEmpty)
        else tail.toRanges(nextIndex, id + 1, cum :+ (index, nextIndex - 1, id), !isEmpty)

  def moveBlocks(dst: IdRanges, src: IdRanges, cum: IdRanges): IdRanges = (dst, src) match
    case ((dstHead @ (dstFrom, dstTo, dstId)) +: dstTail, (srcFrom, srcTo, srcId) +: srcTail) =>
      if dstFrom >= srcFrom then cum
      else if dstId != -1 && dstTo < srcFrom then moveBlocks(dstTail, src, cum :+ dstHead)
      else if srcId == -1 then moveBlocks(dst, srcTail, cum)
      else
        val (srcSize, dstSize) = (srcTo - srcFrom + 1, dstTo - dstFrom + 1)
        val step = Math.min(dstSize, srcSize)
        val newDst = if (dstSize > step) (dstFrom + step, dstTo, dstId) +: dstTail else dstTail
        val newSrc = if (srcSize > step) (srcFrom + step, srcTo, srcId) +: srcTail else srcTail
        moveBlocks(newDst, newSrc, cum :+ (dstFrom, dstFrom + step - 1, srcId))

  def checksum(from: Int, to: Int, id: Int): Long = (from to to).map(_ * id.toLong).sum

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
