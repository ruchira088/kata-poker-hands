package poker.hand

import poker.cards.Card._
import poker.cards.{Ace, Card, Two}

object HighestRankingCards
{
  def apply(pokerHand: PokerHand): PokerHandSummary =
  {
    val cards = pokerHand.cards

    val pokerHandSummaryCreator: (List[Card], PokerHandRanking) => PokerHandSummary =
      PokerHandSummary.create(cards)

    val singleSignificantCardSummaryCreator = (card: Card, ranking : PokerHandRanking) =>
      pokerHandSummaryCreator(List(card), ranking)

    def createComparisonOrder(cardsList: List[Card]): List[Card] = cardsList.sorted ++ cards.diff(cardsList).sorted

    getStraightFlush(cards).map(straightFlush =>
      singleSignificantCardSummaryCreator(straightFlush.tail.head, StraightFlush(straightFlush))
    )
      .orElse(fourOfAKind(cards).map(fourOfAKind =>
        singleSignificantCardSummaryCreator(fourOfAKind.head, FourOfAKind(fourOfAKind)))
      )

      .orElse(getFullHouse(cards).flatMap {
        case threes :: twos :: Nil => Some(singleSignificantCardSummaryCreator(threes.head, FullHouse(threes, twos)))
        case _ => None
      })

      .orElse(getFlush(cards).map(flushCards =>
        pokerHandSummaryCreator(flushCards.sorted, Flush(flushCards.sorted)))
      )

      .orElse(getStraight(cards).map(straightCards =>
        singleSignificantCardSummaryCreator(straightCards.tail.head, Straight(straightCards)))
      )

      .orElse(threeOfAKind(cards).map(threeCards =>
        singleSignificantCardSummaryCreator(threeCards.head, ThreeOfAKind(threeCards)))
      )

      .orElse(twoPair(cards).flatMap {
        case twoPairCards @ pairOne :: pairTwo :: Nil =>
          Some(pokerHandSummaryCreator(createComparisonOrder(twoPairCards.flatten), TwoPair(pairOne, pairTwo)))
        case _ => None
      })

      .orElse(twoOfAKind(cards).map(pairCards =>
        pokerHandSummaryCreator(createComparisonOrder(pairCards), Pair(pairCards)))
      )

      .getOrElse(pokerHandSummaryCreator(cards.sorted, HighCard(cards.sorted.head)))
  }


  private[hand] def getFlush(cards: List[Card]): Option[List[Card]] =
    cards.headOption
      .flatMap(card => if (cards.forall(_.suite == card.suite)) Some(cards) else None)

  private def nOfAKind(n: Int)(cards: List[Card]): Option[List[Card]] =
    cards.groupBy(_.value)
      .toList
      .find { case (_, pairs) => pairs.length >= n }
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
