import cards.Card

object WinningCards
{
  def apply(pokerHand: PokerHand): PokerHandSummary =
  {
    val cards = pokerHand.cards
    val pokerHandSummaryCreator: (List[Card], PokerHandRanking, String) => PokerHandSummary =
      PokerHandSummary.create(cards)

    def orderedRemainingCards(cardsList: List[Card]): List[Card] =
      cards.diff(cardsList).sortBy(_.value)

    def createComparisonOrder(cardsList: List[Card]): List[Card] =
      cardsList.sortBy(_.value) ++ orderedRemainingCards(cardsList)

    isStraightFlush(cards).map(straightFlush =>
      pokerHandSummaryCreator(List(straightFlush.head), StraightFlush, "")
    )
      .orElse(fourOfAKind(cards).map(fourOfAKind =>
        pokerHandSummaryCreator(List(fourOfAKind.head), FourOfAKind, "")))
      .orElse(isFullHouse(cards).map { case threes :: _ =>
        pokerHandSummaryCreator(List(threes.head), FullHouse, "")
      })
      .orElse(getFlush(cards).map(flushCards =>
        pokerHandSummaryCreator(createComparisonOrder(flushCards), Flush, "")))
      .orElse(isStraight(cards).map(straightCards =>
        pokerHandSummaryCreator(List(straightCards.head), Straight, "")))
      .orElse(threeOfAKind(cards).map(threeCards =>
        pokerHandSummaryCreator(List(threeCards.head), ThreeOfAKind, "")))
      .orElse(twoPair(cards).map(twoPairCards =>
        pokerHandSummaryCreator(createComparisonOrder(twoPairCards.flatten), TwoPair, "")))
      .orElse(twoOfAKind(cards).map(pairCards =>
        pokerHandSummaryCreator(createComparisonOrder(pairCards), Pair, "")))
      .getOrElse(pokerHandSummaryCreator(cards.sortBy(_.value), HighCard, ""))
  }


  private def getFlush(cards: List[Card]): Option[List[Card]] =
    cards.headOption
      .flatMap(card => if (cards.forall(_.suite == card.suite)) Some(cards) else None)

  private def getHighCard(cards: List[Card]): Option[Card] = cards match {
    case Nil => None
    case _ => Some(cards.maxBy(_.value))
  }

  private def getPairs(cards: List[Card]): List[(Int, List[Card])] =
    cards.groupBy(_.value)
      .filter { case (_, pairs) => pairs.length > 1 }
      .toList
      .map { case (_, pairs) => (pairs.length, pairs) }

  private def nOfAKind(n: Int)(cards: List[Card]): Option[List[Card]] =
    getPairs(cards)
      .find { case (count, _) => count == n }
      .map { case (_, cardsList) => cardsList }

  private def fourOfAKind(cards: List[Card]): Option[List[Card]] = nOfAKind(4)(cards)

  private def threeOfAKind(cards: List[Card]): Option[List[Card]] = nOfAKind(3)(cards)

  private def twoPair(cards: List[Card]): Option[List[List[Card]]] = for
    {
      pair <- twoOfAKind(cards)
      otherPair <- twoOfAKind(cards.diff(pair))
    } yield List(pair, otherPair)

  private def twoOfAKind(cards: List[Card]): Option[List[Card]] = nOfAKind(2)(cards)

  private def isStraight(cards: List[Card]): Option[List[Card]] = cards.sortBy(_.value).reverse match {
    case x :: y :: rest => if (x.value.intValue - y.value.intValue == 1) isStraight(y :: rest).map(x :: _) else None
    case x => Some(x)
  }

  private def isFullHouse(cards: List[Card]): Option[List[List[Card]]] = for
    {
      threes <- threeOfAKind(cards)
      twos <- twoOfAKind(cards.diff(threes))
    } yield List(threes, twos)

  private def isStraightFlush(cards: List[Card]): Option[List[Card]] =
    isStraight(cards).flatMap(getFlush)
}
