object Day22 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day22.txt")
    println(part1(input))
  }

  case class Brick(x: List[Int], y: List[Int], z: List[Int])

  def parse(input: List[String]): List[Brick] = {
    input.map {
      case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
        Brick(
          (x1.toInt to x2.toInt).toList,
          (y1.toInt to y2.toInt).toList,
          (z1.toInt to z2.toInt).toList
        )
    }
  }

  def part1(input: List[String]): Int = {
    val bricks = parse(input)

    bricks.sortBy(- _.z.max).foreach(println)

    0
  }
}
