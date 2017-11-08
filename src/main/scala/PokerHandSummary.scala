import cards.Card

case class PokerHandSummary(
      cards: List[Card],
      tieBreaker: List[Card],
      ranking: PokerHandRanking,
      description: String
) extends Ordered[PokerHandSummary]
{
  override def compare(that: PokerHandSummary): Int =
    ranking.rankValue - that.ranking.rankValue match {
      case 0 => PokerHandSummary.compareCardList(tieBreaker, that.tieBreaker)
      case x => x
    }
}

object PokerHandSummary
{
  def create(cards: List[Card])(tieBreaker: List[Card], ranking: PokerHandRanking, description: String): PokerHandSummary =
    PokerHandSummary(cards, tieBreaker, ranking, description)

  def compareCardList(cards_1: List[Card], cards_2: List[Card]): Int = (cards_1, cards_2) match
    {
    case (_, Nil) => 0
    case (Nil, _) => 0
    case (Card(_, x) :: _, Card(_, y) :: _) if x != y => x.intValue - y.intValue
    case (_ :: xs, _ :: ys) => compareCardList(xs, ys)

    }


}