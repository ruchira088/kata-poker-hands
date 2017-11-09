package poker.hand

import poker.cards.Card._
import poker.cards.{Ace, Card, Two}

object HighestRankingCards
{
  def apply(pokerHand: PokerHand): PokerHandSummary =
  {
    val cards = pokerHand.cards
    val pokerHandSummaryCreator: (List[Card], PokerHandRanking, String) => PokerHandSummary =
      PokerHandSummary.create(cards)

    def orderedRemainingCards(cardsList: List[Card]): List[Card] =
      cards.diff(cardsList).sorted

    def createComparisonOrder(cardsList: List[Card]): List[Card] =
      cardsList.sorted ++ orderedRemainingCards(cardsList)

    getStraightFlush(cards).map(straightFlush =>
      pokerHandSummaryCreator(List(straightFlush.tail.head), StraightFlush, s"Straight flush: ${printCards(straightFlush)}")
    )
      .orElse(fourOfAKind(cards).map(fourOfAKind =>
        pokerHandSummaryCreator(List(fourOfAKind.head), FourOfAKind, s"Four of a kind: ${printCards(fourOfAKind)}")))
      .orElse(getFullHouse(cards).map { case threes :: twos :: Nil =>
        pokerHandSummaryCreator(List(threes.head), FullHouse, s"Full house: ${printCards(threes)} ${printCards(twos)}")
      })
      .orElse(getFlush(cards).map(flushCards =>
        pokerHandSummaryCreator(createComparisonOrder(flushCards), Flush, s"Flush: ${printCards(flushCards.sorted)}")))
      .orElse(getStraight(cards).map(straightCards =>
        pokerHandSummaryCreator(List(straightCards.tail.head), Straight, s"Straight: ${printCards(straightCards)}")))
      .orElse(threeOfAKind(cards).map(threeCards =>
        pokerHandSummaryCreator(List(threeCards.head), ThreeOfAKind, s"Three of a kind: ${printCards(threeCards)}")))
      .orElse(twoPair(cards).map {
        case twoPairCards @ pairOne :: pairTwo :: Nil =>
          pokerHandSummaryCreator(
            createComparisonOrder(twoPairCards.flatten), TwoPair, s"Two pair: ${printCards(pairOne)} ${printCards(pairTwo)}")
      })
      .orElse(twoOfAKind(cards).map(pairCards =>
        pokerHandSummaryCreator(createComparisonOrder(pairCards), Pair, s"Pair: ${printCards(pairCards)}")))
      .getOrElse(pokerHandSummaryCreator(cards.sorted, HighCard, s"High card: ${cards.sorted.head}"))
  }


  private[hand] def getFlush(cards: List[Card]): Option[List[Card]] =
    cards.headOption
      .flatMap(card => if (cards.forall(_.suite == card.suite)) Some(cards) else None)

  private[hand] def getHighCard(cards: List[Card]): Option[Card] = cards match {
    case Nil => None
    case _ => Some(cards.max)
  }

  private[hand] def getPairs(cards: List[Card]): List[(Int, List[Card])] =
    cards.groupBy(_.value)
      .filter { case (_, pairs) => pairs.length > 1 }
      .toList
      .map { case (_, pairs) => (pairs.length, pairs) }

  private def nOfAKind(n: Int)(cards: List[Card]): Option[List[Card]] =
    getPairs(cards)
      .find { case (count, _) => count >= n }
      .map { case (_, cardsList) => cardsList }

  private[hand] def fourOfAKind(cards: List[Card]): Option[List[Card]] = nOfAKind(4)(cards)

  private[hand] def threeOfAKind(cards: List[Card]): Option[List[Card]] = nOfAKind(3)(cards)

  private[hand] def twoOfAKind(cards: List[Card]): Option[List[Card]] = nOfAKind(2)(cards)

  private[hand] def twoPair(cards: List[Card]): Option[List[List[Card]]] = for
    {
      pair <- twoOfAKind(cards)
      otherPair <- twoOfAKind(cards.diff(pair))
    } yield List(pair, otherPair)

  private[hand] def getStraight(cards: List[Card]): Option[List[Card]] = cards.sorted match {
      case x :: y :: rest if x.value.intValue - y.value.intValue == 1 => getStraight(y :: rest).map(x :: _)
      case x :: rest if x.value == Ace && rest.last.value == Two => getStraight(rest).map(_ :+ x)
      case xs @ _ :: Nil => Some(xs)
      case _ => None
    }

  private[hand] def getFullHouse(cards: List[Card]): Option[List[List[Card]]] = for
    {
      threes <- threeOfAKind(cards)
      twos <- twoOfAKind(cards.diff(threes))
    } yield List(threes, twos)

  private[hand] def getStraightFlush(cards: List[Card]): Option[List[Card]] =
    getStraight(cards).flatMap(getFlush)
}
