import scala.annotation.tailrec

object Day22 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day22.txt")
    println(part1(input))
  }

  case class Brick(x: List[Int], y: List[Int], z: List[Int])

  def parse(input: List[String]): List[Brick] = {
    input.map {
        case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
          Brick(
            (x1.toInt to x2.toInt).toList,
            (y1.toInt to y2.toInt).toList,
            (z1.toInt to z2.toInt).toList
        )
    }
  }

  def part1(input: List[String]): Int = {

    val bricks = parse(input).sortBy(_.z.min)

    @tailrec
    def loop(bricks: List[Brick], processedBricks: List[Int], acc: Int): Int = bricks match
      case Nil =>
        println(processedBricks)
        acc
      case brick :: tail =>
        val xy = for {
          x <- brick.x
          y <- brick.y
        } yield 10 * y + x

        val xyLayer = processedBricks
          .filter(brick => xy.contains(brick % 100))

        xyLayer match
          case Nil => loop(tail, xy.map(_ + 100) ::: processedBricks, acc)
          case _ =>

            val zLayer = xyLayer.max / 100 + brick.z.length

            loop(tail, xy.map(_ + zLayer * 100) ::: processedBricks, acc)

    loop(bricks, List.empty, 0)
  }
}
