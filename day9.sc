import $file.lib.AocDay
import AocDay._

import scala.annotation.tailrec

object Today extends AocDay(9):
  type Block = (Int, Option[Int])
  extension (block: Block)
    def space = block._1
    def id = block._2
    def shrink(step: Int): Block = (block.space - step, block.id)
    def shrinkBy(other: Block) =
      if block.space > other.space then Vector(block.shrink(other.space)) else Vector.empty
    def willFit(other: Block): Boolean = block.id.isEmpty && block.space >= other.space

  def compactBlocks(blocks: Vector[Block], cum: Seq[Block]): Seq[Block] = blocks match
    case init :+ last if last.id.isEmpty   => compactBlocks(init, cum)
    case head +: tail if head.id.isDefined => compactBlocks(tail, cum :+ head)
    case head +: middle :+ last =>
      val newBlocks = head.shrinkBy(last) ++ middle ++ last.shrinkBy(head)
      compactBlocks(newBlocks, cum :+ (Math.min(head.space, last.space), last.id))
    case _ => cum

  def compactFiles(blocks: Vector[Block], cum: Seq[Block]): Seq[Block] =
    blocks match
      case init :+ last if last.id.isEmpty => compactFiles(init, last +: cum)
      case init :+ last =>
        init.indices.find(init(_).willFit(last)) match
          case None => compactFiles(init, last +: cum)
          case Some(i) =>
            val newBlocks = (init.take(i) :+ last) ++ init(i).shrinkBy(last) ++ init.drop(i + 1)
            compactFiles(newBlocks, (last.space, None) +: cum)
      case _ => cum

  def parseAsBlocks(input: String): Vector[Block] =
    val inputGroupedWithId = input.split("").map(_.toInt).toSeq.grouped(2).zipWithIndex
    inputGroupedWithId.toVector.flatMap:
      case (Seq(fileBlock, emptyBlock), id) => Seq((fileBlock, Some(id)), (emptyBlock, None))
      case (Seq(fileBlock), id)             => Seq((fileBlock, Some(id)))
  def renderBlocks(blocks: Seq[Block]) = blocks.flatMap(b => List.fill(b.space)(b.id.getOrElse(0)))
  def checksum(blocks: Seq[Block]) = renderBlocks(blocks).zipWithIndex.map(_.toLong * _).sum

  def part1: AocPart = input => checksum(compactBlocks(parseAsBlocks(input), List.empty))

  def part2: AocPart = input => checksum(compactFiles(parseAsBlocks(input), List.empty))

@main def main(): Unit = Today.solve()
