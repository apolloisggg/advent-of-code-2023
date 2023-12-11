import CharExt.*

// TODO: make it normal
object Day11 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day11.txt")
    val parsed = parse(input)
    println(day11(parsed))
  }

  case class Cell[V](value: V, row: Int, column: Int)

  trait Matrix[V] {
    val value: List[Cell[V]]
    val rows: Int = value.maxBy(_.row).row + 1
    val columns: Int = value.maxBy(_.column).column + 1

    def findColumns(p: V => Boolean): List[Int] = (0 until columns).foldLeft(List.empty)((acc, column) =>
      val cur = value.filter(_.column == column)
      if cur.forall(x => p(x.value)) then column :: acc else acc
    )

    def findRows(p: V => Boolean): List[Int] = (0 until rows).foldLeft(List.empty)((acc, row) =>
      val cur = value.filter(_.row == row)
      if cur.forall(x => p(x.value)) then row :: acc else acc
    )
  }

  case class MatrixChar(value: List[Cell[Char]]) extends Matrix[Char]

  def parse(input: List[String]): List[Cell[Char]] =
    input.zipWithIndex.flatMap { case (str, row) => str.zipWithIndex
      .map { case (char, column) => Cell(char, row, column)
      }
    }

  // Manhattan Distance = | x 1 − x 2 | + | y 1 − y 2 |
  def day11(input: List[Cell[Char]]): BigInt = {
    val expansion = 1000000 - 1 // part 1 = 2 - 1 , part 2 = 1000000 - 1

    val matrix = MatrixChar(input)

    val doubleSpaceR = matrix.findRows(_.isDot)

    val m1 = doubleSpaceR.foldLeft(matrix.value)((mat, r) =>
      val newM = mat.map(cell => if cell.row > r then cell.copy(row = cell.row + expansion) else cell)
      newM
    )

    val doubleSpaceC = matrix.findColumns(_.isDot)

    val m2 = doubleSpaceC.foldLeft(m1)((mat, c) =>
      val newM = mat.map(cell => if cell.column > c then cell.copy(column = cell.column + expansion) else cell)
      newM
    )

    val galaxies = m2.filter(cell => cell.value.isHash)

    galaxies.foreach(println)

    def findDistance(galaxies: List[Cell[Char]], distance: BigInt): BigInt = galaxies match
      case last :: Nil => distance
      case g :: rest =>
        val d: BigInt = rest.foldLeft(BigInt.int2bigInt(0))((acc, cell) => BigInt((g.row - cell.row).abs + (g.column - cell.column).abs) + acc)
        findDistance(rest, distance + d)

    findDistance(galaxies, BigInt.int2bigInt(0))
  }
}
