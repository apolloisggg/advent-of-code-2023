import scala.io.Source

object Util {
  def readLines(fileName: String):Seq[String] = {
    Source.fromResource(fileName).getLines.toSeq
  }
}
