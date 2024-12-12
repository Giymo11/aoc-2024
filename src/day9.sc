
import scala.annotation.tailrec

object Today extends AocDay(9) {
  type Block = (Int, Option[Int])
  extension (block: Block)
    def space = block._1
    def id = block._2
    def shrink(step: Int): Block = (block.space - step, block.id)
    def shrinkBy(other: Block): Seq[Block] =
      if block.space > other.space then Vector(block.shrink(other.space)) else Vector()

  @tailrec def compactBlocks(blocks: Seq[Block], cum: Seq[Block]): Seq[Block] = blocks match
    case init :+ last if last.id.isEmpty   => compactBlocks(init, cum)
    case head +: tail if head.id.isDefined => compactBlocks(tail, cum :+ head)
    case head +: middle :+ last =>
      val newBlocks = head.shrinkBy(last) ++ middle ++ last.shrinkBy(head)
      compactBlocks(newBlocks, cum :+ (Math.min(head.space, last.space), last.id))
    case _ => cum

  @tailrec def compactFiles(blocks: Seq[Block], cum: Seq[Block]): Seq[Block] = blocks match
    case init :+ last if last.id.isEmpty => compactFiles(init, (last.space, None) +: cum)
    case init :+ last =>
      findEmptySpace(init, last) match
        case None                => compactFiles(init, last +: cum)
        case Some(updatedBlocks) => compactFiles(updatedBlocks, (last.space, None) +: cum)
    case _ => cum

  def findEmptySpace(blocks: Seq[Block], other: Block): Option[Seq[Block]] =
    val emptyAt = blocks.indices.find(i => blocks(i).id.isEmpty && blocks(i).space >= other.space)
    emptyAt.map(i => (blocks.take(i) :+ other) ++ blocks(i).shrinkBy(other) ++ blocks.drop(i + 1))

  def parseAsBlocks(input: String): Seq[Block] =
    val inputGroupedWithId = input.split("").filter(_ != "").map(_.toInt).toSeq.grouped(2).zipWithIndex
    inputGroupedWithId.toVector.flatMap:
      case (Seq(fileBlock, emptyBlock), id) => Seq((fileBlock, Some(id)), (emptyBlock, None))
      case (Seq(fileBlock), id)             => Seq((fileBlock, Some(id)))
  def renderBlocks(blocks: Seq[Block]) = blocks.flatMap(b => List.fill(b.space)(b.id.getOrElse(0)))
  def checksum(blocks: Seq[Block]) = renderBlocks(blocks).zipWithIndex.map(_.toLong * _).sum

  def part1: AocPart = input => checksum(compactBlocks(parseAsBlocks(input), Vector.empty))

  def part2: AocPart = input => checksum(compactFiles(parseAsBlocks(input), Vector.empty))
}

Today.solve()
