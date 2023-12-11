object Day10 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day10.txt")
    println(day10_1(input))
  }

  case class Pos(r: Int, c: Int) {
    def left: Pos = Pos(r, c - 1)
    def right: Pos = Pos(r, c + 1)
    def up: Pos = Pos(r - 1, c)
    def down: Pos = Pos(r + 1, c)

    def next(d: Direction): Pos = d match
      case Direction.UP => this.up
      case Direction.DOWN => this.down
      case Direction.LEFT => this.left
      case Direction.RIGHT => this.right
  }

  enum Direction:
    case UP, DOWN, LEFT, RIGHT

  def parse(input: List[String]): List[List[Char]] = input.map(_.toList)

  var visitedPerRow: Map[Int, List[Pos]] = Map.empty

  def day10_1(input: List[String]): Int = {
    val matrix = parse(input)
    val iter = matrix.zipWithIndex.iterator

    // find S point
    var column = -1
    var row = 0

    while column < 0
    do
      val next = iter.next()
      row = next._2
      column = next._1.indexOf('S')

    val s = Pos(row, column)

    // how to find direction from S point ???
    val direction = Direction.UP

    def loop(cur: Pos, steps: Int, direction: Direction): Int = {
      if cur == s && steps > 0 then steps
      else
        val next = cur.next(direction)
        val d = matrix(next.r)(next.c) match
          case '7' if direction == Direction.UP => Direction.LEFT
          case 'F' if direction == Direction.UP => Direction.RIGHT
          case 'L' if direction == Direction.DOWN => Direction.RIGHT
          case 'J' if direction == Direction.DOWN => Direction.LEFT
          case 'J' | 'L' => Direction.UP
          case '7' | 'F' => Direction.DOWN
          case _ => direction
        loop(next, steps + 1, d)
    }

    loop(s, 0, direction) / 2
  }

  // mark visited points ???
  // fill flood algo ???
  def day10_2 = ???

}
