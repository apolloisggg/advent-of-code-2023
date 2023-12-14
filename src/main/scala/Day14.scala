import CharExt.*
import scala.annotation.tailrec

object Day14 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day14.txt")
    println(part1(input))
  }

  case class Cell(value: Char, row: Int, column: Int)

  def parse(input: List[String]): List[Cell] =
    input.zipWithIndex.flatMap { case (str, row) => str.zipWithIndex
      .map { case (char, column) => Cell(char, row, column)
      }
    }

  def part1(input: List[String]): Int = {
    val rows = input.length
    val columns = input.head.length

    @tailrec
    def loop(list: List[Cell], res: List[Cell]): List[Cell] = {
      list match
        case Nil => res
        case head :: tail =>
          if head.value.isO then res.lastIndexWhere(c => c.value.isO || c.value.isHash) match
            case -1 => loop(tail, head.copy(row = 0) :: res.map(c => c.copy(row = c.row + 1)))
            case index =>
              val (left, right) = res.splitAt(index + 1)
              loop(tail, left ::: List(head.copy(row = index + 1)) ::: right.map(c => c.copy(row = c.row + 1)))
          else loop(tail, res :+ head)
    }

    val modified = parse(input).groupBy(_.column).flatMap { case (col, cells) => loop(cells.sortBy(_.row), List.empty[Cell]) }

    (rows to 1 by -1).foldLeft(0)((sum, coeff) => modified.count(x => x.row == rows - coeff && x.value.isO) * coeff + sum)
  }
}
