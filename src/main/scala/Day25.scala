import scala.collection.mutable

object Day25 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day25.txt")
    part1(input)
  }

  type Graph = mutable.Map[String, mutable.Set[String]]

  def graph(input: List[String]): Graph = {
    val graph: Graph = mutable.Map.empty[String, mutable.Set[String]]

    input.foreach {
      case s"$v: $c" =>
        val connected = c.split(" ").to(mutable.Set)
        graph.update(v, graph.getOrElse(v, mutable.Set.empty) ++= connected)
        connected.foreach { con =>
          graph.update(con, graph.getOrElse(con, mutable.Set.empty) += v)
        }
    }

    graph
  }

  def part1(input: List[String]): Int = {
    val g = graph(input)

    g.foreach(println)

    0
  }
}
