import scala.io.{Source, *}
import java.io.*


object Day5 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day5.txt").getLines.mkString(" ")
    println(day5_1(input))
    // println(day5_2(input))
  }

  case class Range(destinationStart: Long, sourceStart: Long, length: Long)

  case class Almanac(seeds: List[Long],
                     seedToSoil: List[Range],
                     soilToFertilizer: List[Range],
                     fertilizerToWater: List[Range],
                     waterToLight: List[Range],
                     lightToTemperature: List[Range],
                     temperatureToHumidity: List[Range],
                     humidityToLocation: List[Range]) {

    def step(value: Int): List[Range] = {
      value match
        case 1 => this.seedToSoil
        case 2 => this.soilToFertilizer
        case 3 => this.fertilizerToWater
        case 4 => this.waterToLight
        case 5 => this.lightToTemperature
        case 6 => this.temperatureToHumidity
        case _ => this.humidityToLocation // final step
    }
  }

  def parse(input: String): Almanac =
    input match
      case s"seeds: $seeds seed-to-soil map: $s2s soil-to-fertilizer map: $s2f fertilizer-to-water map: $f2w water-to-light map: $w2l light-to-temperature map: $l2t temperature-to-humidity map: $t2h humidity-to-location map: $h2l" =>
        Almanac(seeds.split("\\s+").map(_.toLong).toList,
          s2s.toRange, s2f.toRange, f2w.toRange, w2l.toRange, l2t.toRange, t2h.toRange, h2l.toRange)


  def day5_1(input: String): Long = {
    val almanac = parse(input)
    almanac.seeds.map { seed =>
      (1 to 7).foldLeft(seed) { (current, step) =>
        almanac.step(step).find(r => current >= r.sourceStart && current <= r.sourceStart + r.length).fold(current)(r => current - r.sourceStart + r.destinationStart)
      }
    }.min
  }
  
  // TODO: doesnt work OOM error
//  def day5_2(input: String): Long = {
//    val almanac = parse(input)
//
//    almanac.seeds.sliding(2, 2).foreach { seedRange =>
//
//      val seeds = seedRange.head until (seedRange.head + seedRange.last)
//
//      val res = seeds.map { seed =>
//        (1 to 7).foldLeft(seed) { (current, step) =>
//          almanac.step(step).find(r => current >= r.sourceStart && current <= r.sourceStart + r.length).fold(current)(r => current - r.sourceStart + r.destinationStart)
//        }
//      }.min
//    }
//  }

  implicit class StringExt(val str: String) {
    def splitBySpaces: List[Long] = str.split("\\s+").map(_.toLong).toList

    def toRange: List[Range] = str.splitBySpaces
      .sliding(3, 3).map {
        case d :: s :: l :: _ => Range(d, s, l)
      }.toList
  }
}
