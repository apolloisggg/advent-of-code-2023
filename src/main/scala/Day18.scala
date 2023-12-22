object Day18 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day18.txt")
    println(part1(input))
    println(part2(input))
  }

  case class Step(d: Direction, steps: Int)
  case class Point(x: BigInt, y: BigInt)

  type Steps = List[Step]

  // info: https://www.themathdoctors.org/polygon-coordinates-and-areas/
  // Pick's theorem: https://en.wikipedia.org/wiki/Pick%27s_theorem
  def findLavaArea(steps: Steps): BigInt = {
    val polygon = steps.foldLeft(List(Point(0, 0))) { (acc, step) =>
      val point = acc.head
      val cur = step.d match
        case Direction.RIGHT => Point(x = point.x, y = point.y + step.steps)
        case Direction.LEFT => Point(x = point.x, y = point.y - step.steps)
        case Direction.DOWN => Point(x = point.x + step.steps, y = point.y)
        case Direction.UP => Point(x = point.x - step.steps, y = point.y)
      cur :: acc

    } // head and last are the start Point(0.0)

    // Manhattan Distance
    val perimeter = polygon.sliding(2, 1).foldLeft(BigInt(0))((acc, side) => (side.head.x - side.last.x).abs + (side.head.y - side.last.y).abs + acc)

    // Shoelace formula - area of a polygon
    val area = polygon.sliding(2, 1).foldLeft(BigInt(0))((acc, side) => side.head.x * side.last.y - side.head.y * side.last.x + acc).abs / 2

    // interior points using Pick's theorem
    val interior: BigInt = area - perimeter / 2 + 1

    perimeter + interior
  }

  def part1(input: List[String]): BigInt = {
    def parse(input: List[String]): List[Step] = input.map {
      case s"$d $s ($c)" => Step(Direction.get(d.head), s.toInt)
    }

    findLavaArea(parse(input))
  }

  def part2(input: List[String]): BigInt = {
    def parse(input: List[String]): List[Step] = input.map {
      case s"$d $s ($c)" => Step(Direction.get(c.last), Integer.parseInt(c.slice(1, 6), 16))
    }

    findLavaArea(parse(input))
  }

  // TODO: Flood Fill Algorithm ???

}
