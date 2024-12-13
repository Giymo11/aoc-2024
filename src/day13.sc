//> using file ../aoclib

// a.x * ap + b.x * bp = price.x
// a.y * ap + b.y * bp = price.y

type Arcade = Seq[Seq[BigInt]]
extension (arcade: Arcade)
  def calcButtonPresses: Option[(BigInt, BigInt)] = arcade match
    case Seq(a, b, p) =>
      val bPresses = (a(0) * p(1) - a(1) * p(0)) / (a(0) * b(1) - a(1) * b(0))
      val aPresses = (p(1) - b(1) * bPresses) / a(1)
      val res = (a.map(_ * aPresses) zip b.map(_ * bPresses)).map(_ + _)
      if res == p then Some(aPresses, bPresses)
      else None
  def tokenCost: Option[BigInt] = calcButtonPresses.map((a, b) => a * 3 + b)
  def offsetPrize = arcade match
    case Seq(a, b, p) => Seq(a, b, p.map(_ + BigInt("10000000000000")))

object Today extends AocDay(13):
  def parse(in: String) = in.as_integers.map(BigInt(_)).grouped(6).toSeq.map(_.grouped(2).toSeq)
  def part1: AocPart = input => parse(input).flatMap(_.tokenCost).sum
  def part2: AocPart = input => parse(input).map(_.offsetPrize).flatMap(_.tokenCost).sum

Today.solve()
