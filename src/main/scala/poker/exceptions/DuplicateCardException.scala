package poker.exceptions

import poker.cards.Card

case class DuplicateCardException(cards: List[Card]) extends Exception with ConsoleError
{
  override def getMessage = styleErrorMessage(s"Duplicate cards ${Card.printCards(cards)}")
}
