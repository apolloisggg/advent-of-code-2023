import scala.collection.mutable
import scala.collection.mutable.*

object Day15 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day15.txt")
    println(part1(input))
    println(part2(input))
  }

  def parse(input: List[String]): Array[String] = input.head.split(",")

  private def HASH(str: String): Int = str.foldLeft(0)((sum, char) => (char.toInt + sum) * 17 % 256)

  def part1(input: List[String]): Int = parse(input).map(str => HASH(str)).sum

  def part2(input: List[String]): Int = {

    case class Lens(label: String, num: Int)

    val hashmap: mutable.Map[Int, ListBuffer[Lens]] = mutable.Map.empty[Int, ListBuffer[Lens]]

    // TODO: refactor
    parse(input).foreach {
      case s"$label=$num" => hashmap.get(HASH(label)) match
        case Some(value) =>
          val list = value.indexWhere(_.label == label) match
            case -1 => value :+ Lens(label, num.toInt)
            case i => value.updated(i, Lens(label, num.toInt))
          hashmap.update(HASH(label), list)
        case None => hashmap.addOne((HASH(label), ListBuffer(Lens(label, num.toInt))))
      case s"$label-" => hashmap.get(HASH(label)) match
        case Some(value) =>
          value.indexWhere(_.label == label) match
            case -1 => ()
            case i => value.remove(i)
          hashmap.update(HASH(label), value)
        case None => ()
    }

    hashmap.flatMap { case (box, lenses) =>
      lenses.zipWithIndex.map { case (lens, i) =>
        lens.num * (i + 1) * (box + 1)
      }
    }.sum
  }
}
