import scala.annotation.tailrec

object Day22 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day22.txt")
    println(part1(input))
  }

  case class Brick(x: List[Int], y: List[Int], z: List[Int])
  // pos in Cube is Brick`s for each (x, y, Z) equals x + 10 * y + Z where Z is a new Z
  case class Cube(brick: Int, pos: Int)

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
    def loop(bricks: List[Brick], processedBricks: List[Cube], acc: Int, id: Int): Int = bricks match
      case Nil =>
        println(processedBricks)
        acc
      case brick :: tail =>
        val xy = for {
          x <- brick.x
          y <- brick.y
        } yield 10 * y + x

        val xyLayer = processedBricks
          .filter(brick => xy.contains(brick.pos % 100))

        val zLayer = xyLayer.maxByOption(_.pos)

        zLayer match
          case None => loop(tail, xy.map(c => Cube(id, c + 100)) ::: processedBricks, acc, id + 1)
          case Some(level) =>

            val z = level.pos / 100 + brick.z.length

            val replaceable = xyLayer.filter(_.pos >= level.pos / 100 * 100).map(_.brick).distinct match
              case head :: Nil => acc
              case x => x.length + acc

            println(xyLayer.filter(_.pos >= level.pos / 100 * 100))
            println(xyLayer.filter(_.pos >= level.pos / 100 * 100).map(_.brick).distinct)

            loop(tail, xy.map(c => Cube(id, c + z * 100)) ::: processedBricks, replaceable, id + 1)

    loop(bricks, List.empty, 0, 1)
  }
}
