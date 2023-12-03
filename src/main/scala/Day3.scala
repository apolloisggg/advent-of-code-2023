object Day3 {

  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day3.txt")
    println(day3_1(input)) // 544664
    // TODO: go back here
    println(day3_2(input)) // 84495585

  }

  val numberR = "\\d{2,}".r.unanchored

  def day3_1(input: Seq[String]): Int = {

    input.zipWithIndex.flatMap((row, index) => {
      numberR.findAllMatchIn(row).foldLeft(List.empty[Int]) { (list, matcher) =>
        val start = matcher.start
        val end = matcher.end

        val up = if (input.isDefinedAt(index - 1)) input(index - 1).substring(
            if (row.isDefinedAt(start - 1)) start - 1 else start,
            if (row.isDefinedAt(end + 1)) end + 1 else end).forall(_ == '.') else true

        val under = if (input.isDefinedAt(index + 1)) input(index + 1).substring(
            if (row.isDefinedAt(start - 1)) start - 1 else start,
            if (row.isDefinedAt(end + 1)) end + 1 else end).forall(_ == '.') else true

        val left = if (row.isDefinedAt(start - 1)) row(start - 1) == '.' else true

        val right = if (row.isDefinedAt(end)) row(end) == '.' else true

        if (up && under && left && right) list else matcher.matched.toInt :: list
      }
    }).sum
  }

  def day3_2(input: Seq[String]): Long = {
    val asteriskR = "\\*".r
    input.zipWithIndex.flatMap { (row, index) =>
      println(s"row $index")
      val r = asteriskR.findAllMatchIn(row).map { matcher =>

        val gear = matcher.start
        val left = if (row(gear - 1).isDigit) List(row.substring(gear - 3, gear).filter(_.isDigit).toInt) else List.empty
        val right = if (row(gear + 1).isDigit) List(row.substring(gear + 1, gear + 4).filter(_.isDigit).toInt) else List.empty

        val up = numberR.findAllMatchIn(input(index - 1).substring(gear - 3, gear + 4)).filter { m =>
          // println(s"up: matched = ${m.matched} start = ${m.start} end = ${m.end} gear = $gear")
          m.end == 3 || m.start == 3 || m.end == 4 || m.start == 4 || m.start == 2 || m.end == 2 & m.start == 5 || m.end == 5
        }.map(_.matched.toInt).toList

        val down = numberR.findAllMatchIn(input(index + 1).substring(gear - 3, gear + 4)).filter { m =>
          // println(s"down: matched = ${m.matched} start = ${m.start} end = ${m.end} gear = $gear")
          m.end == 3 || m.start == 3 || m.end == 4 || m.start == 4 || m.start == 2 || m.end == 2 & m.start == 5 || m.end == 5
        }.map(_.matched.toInt).toList

        val res = left ::: right ::: up ::: down
        println(res)
        if (res.size == 2) res.head.toLong * res.last.toLong else 0L
      }
      r.toList
    }.sum
  }
}
