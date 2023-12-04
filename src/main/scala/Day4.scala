object Day4 {
  def main(args: Array[String]): Unit = {
    val input = Util.readLines("day4.txt")
    println(day4_1(input))
    println(day4_2(input))
  }

  case class Card(cardNumber: Int, winningNumbers: List[Int], givenNumbers:List[Int])

  def parse(input: Seq[String]): Seq[Card] = {
    val cardR = "Card\\s+(\\d+): ([\\d\\s]+) \\| ([\\d\\s]+)".r.unanchored
    input.map {
      case cardR(cardNumber, winningNumbersStr, givenNumbersStr) =>
        Card(cardNumber.toInt,
          winningNumbers = winningNumbersStr.strip().split("\\s+").map(_.toInt).toList,
          givenNumbers = givenNumbersStr.strip().split("\\s+").map(_.toInt).toList)
    }
  }

  def day4_1(input: Seq[String]): Int = {
    parse(input).map { card =>
      card.givenNumbers.intersect(card.winningNumbers).foldLeft(0)((acc, _) => if (acc == 0) 1 else acc * 2)
    }.sum
  }

  def day4_2(input: Seq[String]): Int = {
    val cards = parse(input)
    val scratchcards = cards.map(x => x.cardNumber -> 1).to(collection.mutable.Map)

    cards.foreach { card =>
      val winningCardsCount: Int = card.givenNumbers.intersect(card.winningNumbers).size
      ((card.cardNumber + 1) to (card.cardNumber + winningCardsCount)).foreach { nextScratchcard =>
        scratchcards.update(nextScratchcard, scratchcards(card.cardNumber) + scratchcards(nextScratchcard))
      }
    }

    scratchcards.values.sum
  }
}
