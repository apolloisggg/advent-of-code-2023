import java.math.BigDecimal

object Day6 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day6.txt")
    val race = parse(input)
    // println(day6_1(race)) // 2065338

    println(day6_2(race)) // 34934171
  }

  case class Race(time: List[Int], distance: List[Int])

  def parse(input: List[String]) =
    input match
      case timeStr :: distanceStr :: _ =>
        val time = timeStr match
          case s"Time: $t" => t.strip().split("\\s+").map(_.toInt).toList

        val distance  = distanceStr match
          case s"Distance: $d" => d.strip().split("\\s+").map(_.toInt).toList

        Race(time, distance)

  def day6_1(race: Race): Int = {
    race.time.zip(race.distance).map { case (time, distance) =>
      val speeds = 1 until time
      speeds.foldLeft(0){ case (winningSpeeds, speed) =>
        val currentDistance = (time - speed) * speed
        if (currentDistance > distance) winningSpeeds + 1 else winningSpeeds
      }
    }.product
  }

  // TODO try more math approach
  def day6_2(race: Race): Long = {
    val totalTime = race.time.map(_.toString).reduce(_ + _).toInt
    val totalDistance = BigInt(race.distance.map(_.toString).reduce(_ + _).toLong)

    val speeds = 1 until totalTime

    speeds.foldLeft(0L) { case (winningSpeeds, speed) =>
      val currentDistance = BigInt(totalTime - speed) * BigInt(speed)
      if (currentDistance > totalDistance) winningSpeeds + 1 else winningSpeeds
    }
  }
}
