package poker.exceptions

import poker.cards.Card

case class IncorrectNumberOfCardsException(cards: List[Card]) extends Exception
