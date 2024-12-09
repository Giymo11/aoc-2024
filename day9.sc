import $file.lib.AocDay
import AocDay._

import scala.annotation.tailrec

object Today extends AocDay(9) {
  type IdRanges = Seq[(Int, Int, Int)]

  type Block = (Int, Option[Int])
  extension (block: Block)
    def space = block._1
    def id = block._2
    def shrink(step: Int) = (block.space - step, block.id)

  def compactBlocks(blocks: Vector[Block], cum: List[Block]): List[Block] = blocks match
    case init :+ last if last.id.isEmpty   => compactBlocks(init, cum)
    case head +: tail if head.id.isDefined => compactBlocks(tail, cum :+ head)
    case head +: middle :+ last =>
      val step = Math.min(head.space, last.space)
      val newBlocks =
        if head.space > last.space then head.shrink(step) +: middle
        else if head.space < last.space then middle :+ last.shrink(step)
        else middle
      compactBlocks(newBlocks, cum :+ (step, last.id))
    case _ => cum

  def part1: AocPart = input =>
    val numbers = input.split("").map(_.toInt).toSeq
    val ranges: Vector[Block] = numbers.grouped(2).zipWithIndex.toVector.flatMap {
      case (Seq(fileBlock, emptyBlock), id) => Seq((fileBlock, Some(id)), (emptyBlock, None))
      case (Seq(fileBlock), id)             => Seq((fileBlock, Some(id)))
    }

    def renderBlocks(blocks: Seq[Block]) =
      blocks.flatMap(block => List.fill(block.space)(block._2.getOrElse(0)))

    def checksum(blocks: Seq[Block]) = renderBlocks(blocks).zipWithIndex.map(_.toLong * _).sum

    val compacted = compactBlocks(ranges, List.empty)
    checksum(compacted)

  def part2: AocPart = input => ""
}

@main def main(): Unit = Today.solve()
