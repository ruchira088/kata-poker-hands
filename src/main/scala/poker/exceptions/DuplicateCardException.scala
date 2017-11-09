package poker.exceptions

import poker.cards.Card

case class DuplicateCardException(cards: List[Card]) extends Exception
{
  override def getMessage = s"Duplicate cards: ${Card.printCards(cards)}"
}
