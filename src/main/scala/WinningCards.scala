import cards.Card

object WinningCards
{
  def getWinningCombo(pokerHand: PokerHand) =
  {
    val cards = pokerHand.cards

    def remainingCards(cardsList: List[Card]): List[Card] = cards.diff(cardsList)

    isStraightFlush(cards).map(???)
      .orElse(fourOfAKind(cards).map(???))
      .orElse(isFullHouse(cards).map(???))
      .orElse(getFlush(cards).map(???))
      .orElse(isStraight(cards).map(???))
      .orElse(threeOfAKind(cards).map(threeCards => PokerHandSummary(8, "")))
      .orElse(twoPair(cards).map(twoPairCards => PokerHandSummary(7, "")))
      .orElse(twoOfAKind(cards).map(pairCards => PokerHandSummary(6, "")))
      .orElse(getHighCard(cards).map(highCard => PokerHandSummary(5, "")))
  }


  def getFlush(cards: List[Card]): Option[List[Card]] =
    cards.headOption
      .flatMap(card => if (cards.forall(_.suite == card.suite)) Some(cards) else None)

  def getHighCard(cards: List[Card]): Option[Card] = cards match {
    case Nil => None
    case _ => Some(cards.maxBy(_.value))
  }

  def getPairs(cards: List[Card]): List[(Int, List[Card])] =
    cards.groupBy(_.value)
      .filter { case (_, pairs) => pairs.length > 1 }
      .toList
      .map { case (_, pairs) => (pairs.length, pairs) }

  def nOfAKind(n: Int)(cards: List[Card]): Option[List[Card]] =
    getPairs(cards)
      .find { case (count, _) => count == n }
      .map { case (_, cardsList) => cardsList }

  def fourOfAKind(cards: List[Card]): Option[List[Card]] = nOfAKind(4)(cards)

  def threeOfAKind(cards: List[Card]): Option[List[Card]] = nOfAKind(3)(cards)

  def twoPair(cards: List[Card]): Option[List[List[Card]]] = for
    {
      pair <- twoOfAKind(cards)
      otherPair <- twoOfAKind(cards.diff(pair))
    } yield List(pair, otherPair)

  def twoOfAKind(cards: List[Card]): Option[List[Card]] = nOfAKind(2)(cards)

  def isStraight(cards: List[Card]): Option[List[Card]] = cards.sortBy(_.value).reverse match {
    case x :: y :: rest => if (x.value.value - y.value.value == 1) isStraight(y :: rest).map(x :: _) else None
    case x => Some(x)
  }

  def isFullHouse(cards: List[Card]): Option[List[List[Card]]] = for
    {
      threes <- threeOfAKind(cards)
      twos <- twoOfAKind(cards.diff(threes))
    } yield List(threes, twos)

  def isStraightFlush(cards: List[Card]): Option[List[Card]] =
    isStraight(cards).flatMap(getFlush)

}
