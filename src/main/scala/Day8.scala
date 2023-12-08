import scala.annotation.tailrec

object Day8 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day8.txt")
    // println(day8_1(input))
    println(day8_2(input))
  }

  case class Node(left: String, right: String) {
    def direction(d: Char): String = if (d == 'L') this.left else this.right
  }

  def parse(input: List[String]): Map[String, Node] = {
    val nodeR = """(\w{3}) = \((\w{3}), (\w{3})\)""".r.unanchored
    input.flatMap { str =>
      nodeR.findFirstMatchIn(str).map(x => x.group(1) -> Node(x.group(2), x.group(3)))
    }.toMap
  }

  // least common multiply
  // chinese
  def day8_1(input: List[String]): Int = {
    val lf = input.head
    val nodes = parse(input)

    @tailrec
    def find(directions: List[Char], steps: Int, current: Node): Int = directions match
      case Nil => find(lf.toList, steps, current)
      case d :: tail =>
        if (current.direction(d) == "ZZZ") steps + 1
        else find(tail, steps + 1, nodes(current.direction(d)))

    find(lf.toList, 0, nodes("AAA"))
  }

  // least common multiply
  // chinese
  def day8_2(input: List[String]): Int = {
    val lf = input.head
    val nodes = parse(input)

    def find(directions: List[Char], steps: Int, current: Node): Int = directions match
      case Nil => find(lf.toList, steps, current)
      case d :: tail =>
        if (current.direction(d).endsWith("Z")) steps + 1
        else find(tail, steps + 1, nodes(current.direction(d)))

    // fins a lcm
    val steps = nodes.filter((cur, _) => cur.endsWith("A")).values.toList.map { node =>
      find(lf.toList, 0, node)
    }

    println(steps)

    0

  }
}
