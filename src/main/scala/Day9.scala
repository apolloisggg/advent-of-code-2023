import scala.annotation.tailrec

object Day9 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day9.txt")
    println(day9(input))
  }

  def parse(input: List[String]): List[List[Int]] = {
    input.map(i => i.split(" ").map(_.toInt).toList)
  }

  def day9(input: List[String]): Int = {
    val data = parse(input)

    @tailrec
    def loop(matrix: List[List[Int]], current: List[Int]): Int = {
      if (current.forall(_ == 0)) matrix.foldLeft(0) {
        (cur, row) =>
          // cur + row.last // part 1
          row.head - cur  // part 2
      }
      else {
        val next = current.zip(current.tail).map {
          case (x, x_1) => x_1 - x
        }
        loop(current :: matrix, next)
      }
    }

    data.map {
      record => loop(List.empty[List[Int]], record)
    }.sum
  }
}
