import scala.annotation.tailrec
import scala.util.matching.Regex

object Day19 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day19.txt")
    // println(part1(input))
    println(part2(input))
  }

  case class Rating(x: Int, m: Int, a: Int, s: Int) {
    def sum: Int = x + m + a + s
  }

  type Outcome = Rating => String
  type Ratings = List[Rating]
  type Workflows = Map[String, Outcome]
  val Rule: Regex = "(.)([>|<])(\\d+):(\\w+)".r

  def parse(input: List[String]): (Ratings, Workflows) = {

    def outcome(set: List[String])(r: Rating): String = {

      def compare(sign: String, x: Int, y: Int): Boolean = sign match
        case ">" => x > y
        case _ =>  x < y

      @tailrec
      def loop(set: List[String]): String = set match
        case last :: Nil => last
        case head :: tail => head match
          case Rule(cat, sign, number, res) =>
            val x = cat match
              case "x" => r.x
              case "m" => r.m
              case "a" => r.a
              case "s" => r.s
            compare(sign, x, number.toInt) match
              case true => res
              case false => loop(tail)

      loop(set)
    }

    val ratings = input.collect {
      case s"{x=$x,m=$m,a=$a,s=$s}" => Rating(x.toInt, m.toInt, a.toInt, s.toInt)
    }

    val workflows = input.collect {
      case s"$name{$rules}" => name -> outcome(rules.split(",").toList)
    }.toMap

    (ratings, workflows)
  }



  def part1(input: List[String]): Int = {
    val (ratings, workflows) = parse(input)

    def AorR(r: Rating): String = {
      def loop(name: String): String = {
        workflows(name)(r) match
          case res@("R" | "A") => res
          case next => loop(next)
      }

      loop("in")
    }

    ratings.foldLeft(0)((acc, r) => AorR(r) match
      case "R" => acc
      case "A" => r.sum + acc
    )
  }

  def part2(input: List[String]): BigInt = {

    0
  }
}
