package poker.hand

import poker.cards.Card

case class PokerHandSummary(
      cards: List[Card],
      tieBreaker: List[Card],
      ranking: PokerHandRanking,
      description: String
) extends Ordered[PokerHandSummary]
{
  override def compare(that: PokerHandSummary): Int =
    ranking.rankValue - that.ranking.rankValue match {
      case 0 => Card.compareCardList(tieBreaker, that.tieBreaker)
      case x => x
    }
}

object PokerHandSummary
{
  def create(cards: List[Card])(tieBreaker: List[Card], ranking: PokerHandRanking, description: String): PokerHandSummary =
    PokerHandSummary(cards, tieBreaker, ranking, description)
}