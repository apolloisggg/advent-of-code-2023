object Day13 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day13.txt")
    parse(input).foreach(println)
  }

  def parse(input: List[String]): Array[List[String]] = {
    input.mkString("\n").split("\n\n").map(x => x.split("\n").toList)
  }

  def verticalLine(list: List[String]): Option[Int] = {
    val size = list.size

    val rotated = (0 until size).map { index =>
      list.foldLeft("")((l, s) => l(index) + s)
    }

    ???
  }

  def horizontalLine(list: List[String]): Int = {
    list
    ???
  }

  def part1(input: List[String]): Int = {
    parse(input).map { image =>

    }
    
    0
  }
}
