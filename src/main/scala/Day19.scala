import jdk.internal.util.xml.impl.Input

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day19 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day19.txt")
    println(part1(input))
    // println(part2(input))
  }

  case class Rating(x: Int, m: Int, a: Int, s: Int) {
    def sum: Int = x + m + a + s
  }

  type Outcome = Rating => String
  type Ratings = List[Rating]
  type Workflows = Map[String, Outcome]
  type Rules = Map[String, List[String]]
  val Rule: Regex = "(.)([>|<])(\\d+):(\\w+)".r

  def parse(input: List[String]): (Ratings, Rules) = {
    val ratings: Ratings = input.collect {
      case s"{x=$x,m=$m,a=$a,s=$s}" => Rating(x.toInt, m.toInt, a.toInt, s.toInt)
    }

    val rules = input.collect {
      case s"$name{$rules}" => name -> rules.split(",").toList
    }.toMap

    (ratings, rules)
  }

  def part1(input: List[String]): Int = {

    def workflow(set: List[String])(r: Rating): String = {

      def compare(sign: String, x: Int, y: Int): Boolean = sign match
        case ">" => x > y
        case _ => x < y

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
            if compare(sign, x, number.toInt) then
              res
            else
              loop(tail)

      loop(set)
    }

    val (ratings, rules) = parse(input)

    val workflows = rules.map((k, v) => k -> workflow(v))

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

  //px{a<2006:qkq,m>2090:A,rfg}
  //pv{a>1716:R,A}
  //lnx{m>1548:A,A}
  //rfg{s<537:gd,x>2440:R,A}
  //qs{s>3448:A,lnx}
  //qkq{x<1416:A,crn}
  //crn{x>2662:A,R}
  //in{s<1351:px,qqz}
  //qqz{s>2770:qs,m<1801:hdj,R}
  //gd{a>3333:R,R}
  //hdj{m>838:A,pv}

//  def part2(input: List[String]): BigInt = {
//    val (_, rules) = parse(input)
//    val start = Rating(4000, 4000, 4000, 4000)
//
//    def updateRatingValue(sign: String, cur: Int, bound: Int): Int = sign match
//      case ">" => cur - bound - 1
//      case "<" => bound - 1
//
//    def loop(name: String, rating: Rating): Long = {
//
//      rules(name) match
//        case Nil => rating
//        case head :: tail => head match
//          case Rule(cat, sign, number, res) =>
//            val left = cat match
//              case "x" => rating.copy()
//              case "m" => rating.copy()
//              case "a" => rating.copy()
//              case "s" => rating.copy()
//
//            val right = cat match
//              case "x" => rating.copy()
//              case "m" => rating.copy()
//              case "a" => rating.copy()
//              case "s" => rating.copy()
//
//            val s = sign match
//              case ">" => ???
//              case "<" => ???
//
//
//    }
//
//    loop("in", start)
//
//    0
//  }
}
