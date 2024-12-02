// to run this script: amm -watch day-1.sc

import os._

@main
def main(): Unit =
  println("Hello AoC")
  val wd = os.pwd / "aoc-input"

  val test_input = os.read(wd / "test1.txt")
  assert(part1(test_input) == "11")
  assert(part2(test_input) == "31")

  val input1 = os.read(wd / "input1.txt")
  println("part1: " + part1(input1))
  println("part2: " + part2(input1))

def part2(input: String): String =
  val (left, right) = parse_into_two_lists(input)

  val occurance_list = right.groupBy(identity).view.mapValues(_.size)
  val lookup_map = occurance_list.toMap.withDefaultValue(0)

  val similarity_list = left.map(item => item * lookup_map(item))
  return similarity_list.sum.toString()

def part1(input: String): String =
  val (left, right) = parse_into_two_lists(input)

  def calc_distance = (a: Int, b: Int) => Math.abs(a - b)
  val distance_list = left.sorted.zip(right.sorted).map(calc_distance(_, _))

  return distance_list.sum.toString()

import scala.util.matching.Regex
def parse_into_two_lists(input: String): (List[Int], List[Int]) =
  val pattern: Regex = """(\d+)\s+(\d+)""".r
  return input.linesIterator
    .collect { case pattern(num1, num2) =>
      (num1.toInt, num2.toInt)
    }.toList.unzip
