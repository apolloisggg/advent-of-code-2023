import scala.collection.mutable

object Day21 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day19.txt")
  }

  case class Point(x: Int, y: Int)

  def parse(input: List[String]): List[(List[(Char, Int)], Int)] = {
    input.map(_.toList.zipWithIndex).zipWithIndex
  }

  def part1(input: List[String]): Int = {
    val garden = parse(input)
    var queue = mutable.Queue.empty[Point]
    val start = garden
      
    0
  }
}
