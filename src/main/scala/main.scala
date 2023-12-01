@main
def main(): Unit = {
  val input = Util.readLines("day1.txt")
   val res2 = day1_1(day1_2(input))
   println(res2)
}

def day1_1(input: Seq[String]) = {
  input.map { str =>
    val res = for {
      f <- str.find(_.isDigit).map(_.toString.toInt)
      l <-  str.findLast(_.isDigit).map(_.toString.toInt)
    } yield f * 10 + l
    res.getOrElse(0)
  }.sum
}

def day1_2(input: Seq[String]): Seq[String] = {

  val digits = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9"
  )

  input.map {
    str => str.foldLeft("")((res, char) =>
        val r = res.concat(char.toString)
        digits.keys.find(x => r.contains(x)) match
          case None => r
          case Some(key) => r.replace(key, digits(key)).concat(r.last.toString))
  }
}


/**
 * One thing that may not be visible in the test data is that "...eightwo..." should result in [.. 8, 2..]
 *
 */