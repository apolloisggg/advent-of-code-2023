import CharExt.*

object Day12 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day12.txt")
    part1(input)
  }

  case class Record(value: String, groups: List[Int])

  def parse(input: List[String]): List[Record] = {
    input.map{ case s"$s $d" => Record(s, d.split(",").map(_.toInt).toList)}
  }

  def part1(input: List[String]): Int = {
    val records = parse(input)

    records.foreach(println)

    val groupR = "[?|#]+".r.unanchored
    records.foreach { r =>
      val matched = groupR.findAllMatchIn(r.value).map(x => x.matched).toList
      println(matched)
      println(matched.flatten)
      println(r.groups)
    }
    0
  }
}
