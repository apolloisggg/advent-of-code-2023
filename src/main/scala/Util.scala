import scala.io.Source

object Util {
  def readLines(fileName: String): List[String] = {
    Source.fromResource(fileName).getLines.toList
  }
}
