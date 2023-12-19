object Day17 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day16.txt")
  }

  def parse(input: List[String]): List[List[Int]] = input.map(x => x.map(_.intValue).toList)

  // Minimum Cost Path Problem
  // A*
  // bfs
  // Dijkstra's algorithm
  def part1(input: List[List[Int]]): Int = {
    0
  }
}
