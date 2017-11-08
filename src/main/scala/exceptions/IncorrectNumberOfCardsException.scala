package exceptions

import cards.Card

case class IncorrectNumberOfCardsException(cards: List[Card]) extends Exception
