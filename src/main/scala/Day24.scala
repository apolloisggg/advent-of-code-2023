import scala.annotation.tailrec

case class Hailstone(x: Long, y: Long, z: Long, dx: Int, dy: Int, dz: Int) {

  // slope-intercept line: y = ax + b
  val slope: BigDecimal = dy / BigDecimal(dx.toDouble)
  val intercept: BigDecimal = y - slope * x

  def intersection(that: Hailstone): Option[Point] = {
    // parallel
    if this.slope == that.slope then None
    else
      val x = (that.intercept - this.intercept) / (this.slope - that.slope)
      val y = this.slope * x + this.intercept
      Some(Point(x, y))
  }

  def time(point: Point): BigDecimal = (point.x - this.x) / dx

}

case class Point(x: BigDecimal, y: BigDecimal)

object Day24  {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day24.txt")
    println(part1(input))
  }

  def parse(input: List[String]): List[Hailstone] = {
    input.map { str => str.filterNot(_.isWhitespace) match
      case s"$x,$y,$z@$dX,$dY,$dZ" =>
        Hailstone(x.toLong, y.toLong, z.toLong, dX.toInt, dY.toInt, dZ.toInt)
    }
  }

  // Problem: https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
  // SO: https://stackoverflow.com/questions/20677795/how-do-i-compute-the-intersection-point-of-two-lines/20677983#20677983
  // linear function from 2 points: https://www.omnicalculator.com/math/line-equation-from-two-points#:~:text=The%20linear%20equation%20from%20two%20points%20(x1%2C%20y1,points%20in%20three%2Ddimensional%20space.
  def part1(input: List[String]): Long = {
    val min: Long = 200000000000000L
    val max: Long = 400000000000000L
    val hailstones = parse(input)

    @tailrec
    def countIntersection(hs: List[Hailstone], points: List[Point]): Int = hs match
      case last :: Nil => points.length
      case head :: tail =>
        val intersectPoints = tail.foldLeft(points) {
          case (acc, p) => head.intersection(p) match
            case None => acc // paths are parallel
            case Some(intersect) if intersect.x < min || intersect.x > max || intersect.y < min || intersect.y > max => acc // outside the test area
            case Some(intersect) if head.time(intersect) < 0 || p.time(intersect) < 0 => acc // paths crossed in the past
            case Some(intersect) =>
              intersect :: acc
        }
        countIntersection(tail, intersectPoints)

    countIntersection(hailstones, List.empty)
  }
}
