import Day7.Hand.jokerCardRank

object Day7 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day7.txt")
    println(day7_1(input)) // 248836197
    println(day7_2(input)) // 251195607
  }

  case class Hand(value: String, bid: Int) {

    private val cards = value
      .groupBy(identity)
      .map { case (card, count) => card -> count.length }

    private val firstOrderingRule: Int = cards.values.toList.sorted(Ordering.Int.reverse) match
      case 5 :: Nil => 7
      case 4 :: _ => 6
      case 3 :: 2 :: Nil => 5
      case 3 :: 1 :: 1 :: Nil => 4
      case 2 :: 2 :: 1 :: Nil => 3
      case 2 :: _ => 2
      case _ => 1

    def secondOrderingRule(that: Hand)(cardRank: Map[Char, Int]): Int = this.value.lazyZip(that.value)
      .find((x, y) => x != y)
      .map((x, y) => cardRank(x) compare cardRank(y))
      .getOrElse(0)

    def playJoker: Hand =
      if (value.contains('J') && cards('J') != 5)

        val bestCard = cards.removed('J')
          .reduce( (max, cur) =>
            if (cur._2 > max._2) cur
            else if (max._2 > cur._2) max
            else if (jokerCardRank(cur._1) > jokerCardRank(max._1)) cur
            else max)._1

        this.copy(value = this.value.replace('J', bestCard))
      else this
  }

  object Hand {

    private val cardRank: Map[Char, Int] = "23456789TJQKA".zip(1 to 13).toMap
    private val jokerCardRank: Map[Char, Int] = "J23456789TQKA".zip(1 to 13).toMap

    implicit val ordering: Ordering[Hand] = (x, y) =>
      if (x.firstOrderingRule == y.firstOrderingRule) x.secondOrderingRule(y)(cardRank)
      else x.firstOrderingRule compare y.firstOrderingRule

    implicit val orderingJoker: Ordering[Hand] = (x, y) => {
      val jokerX = x.playJoker.firstOrderingRule
      val jokerY = y.playJoker.firstOrderingRule
        if (jokerX == jokerY) x.secondOrderingRule(y)(jokerCardRank)
        else jokerX compare jokerY
    }

    def totalWinning(hands: List[Hand])(implicit ordering: Ordering[Hand]): Int = hands.sorted.zipWithIndex
      .foldLeft(0) { case (acc, (hand, rank)) => hand.bid * (rank + 1) + acc
    }
  }

  def parse(input: List[String]): List[Hand] = input.map {
      case s"$hand $bid" => Hand(hand, bid.toInt)
  }

  def day7_1(input: List[String]): Int = Hand.totalWinning(parse(input))(Hand.ordering)
  def day7_2(input: List[String]): Int = Hand.totalWinning(parse(input))(Hand.orderingJoker)
}