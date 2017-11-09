package poker.exceptions

import poker.cards.Card

case class IncorrectNumberOfCardsException(cards: List[Card]) extends Exception with ConsoleError
{
  override def getMessage = styleErrorMessage(s"Incorrect number of cards: ${Card.printCards(cards)}")
}
