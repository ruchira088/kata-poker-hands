package poker.hand

import poker.cards.Card
import poker.cards.Card.printCards

trait PokerHandRanking extends Ordered[PokerHandRanking]
{
  def rankValue: Int

  def description: String

  override def compare(that: PokerHandRanking): Int = rankValue - that.rankValue
}

case class HighCard(card: Card) extends PokerHandRanking {
  override def rankValue: Int = 5

  override def description = s"High card: $card"
}

case class Pair(pairCards: List[Card]) extends PokerHandRanking {
  override def rankValue: Int = 6

  override def description = s"Pair: ${printCards(pairCards)}"
}

case class TwoPair(pairOne: List[Card], pairTwo: List[Card]) extends PokerHandRanking {
  override def rankValue: Int = 7

  override def description = s"Two pair: ${printCards(pairOne)} ${printCards(pairTwo)}"
}

case class ThreeOfAKind(threeCards: List[Card]) extends PokerHandRanking {
  override def rankValue: Int = 8

  override def description = s"Three of a kind: ${printCards(threeCards)}"
}

case class Straight(straightCards: List[Card]) extends PokerHandRanking {
  override def rankValue: Int = 9

  override def description = s"Straight: ${printCards(straightCards)}"
}

case class Flush(flushCards: List[Card]) extends PokerHandRanking {
  override def rankValue: Int = 10

  override def description = s"Flush: ${printCards(flushCards)}"
}

case class FullHouse(threes: List[Card], twos: List[Card]) extends PokerHandRanking {
  override def rankValue: Int = 11

  override def description = s"Full house: ${printCards(threes)} ${printCards(twos)}"
}

case class FourOfAKind(fourOfAKind: List[Card]) extends PokerHandRanking {
  override def rankValue: Int = 12

  override def description = s"Four of a kind: ${printCards(fourOfAKind)}"
}

case class StraightFlush(straightFlush: List[Card]) extends PokerHandRanking {
  override def rankValue: Int = 13

  override def description = s"Straight flush: ${printCards(straightFlush)}"
}