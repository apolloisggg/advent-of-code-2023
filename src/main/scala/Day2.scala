import scala.util.matching.Regex

object Day2 {

  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day2.txt")
    println(day2_1(input))
    println(day2_2(input))
  }

  case class GameSet(red: Int, green: Int, blue: Int)
  case class Game(number: Int, sets: List[GameSet])
  val possibleGame: GameSet = GameSet(12, 13, 14)

  def parse(input: Seq[String]): Seq[Game] = {

    val gameNumberR = "Game (\\d+)".r.unanchored
    val redR = "(\\d+) red".r.unanchored
    val blueR = "(\\d+) blue".r.unanchored
    val greenR = "(\\d+) green".r.unanchored

    def find[R <: Regex](r: R, data: String): Int = r.findFirstMatchIn(data).map(_.group(1).toInt).getOrElse(0)

    input.map { str =>
      val game :: sets :: _ = str.split(":").toList
      val gameNumber = find(gameNumberR, game)
      val gameSets = sets.split(";").map { set =>
        GameSet(find(redR, set), find(greenR, set), find(blueR, set))
      }.toList

      Game(gameNumber, gameSets)
    }
  }

  def day2_1(input: Seq[String]): Int = {
   parse(input)
     .filter(x => x.sets.forall(y =>
       y.red <= possibleGame.red &&
       y.green <= possibleGame.green &&
       y.blue <= possibleGame.blue))
     .map(_.number).sum
  }

  def day2_2(input: Seq[String]): Int = {
    parse(input).map { game =>
      val red = game.sets.map(_.red).max
      val green = game.sets.map(_.green).max
      val blue = game.sets.map(_.blue).max
      red * green * blue
    }.sum
  }
}
