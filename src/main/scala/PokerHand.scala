import cards.Card
import exceptions.IncorrectNumberOfCardsException

import scala.util.{Failure, Success, Try}

case class PokerHand(card_1: Card, card_2: Card, card_3: Card, card_4: Card, card_5: Card)
{
  def cards: List[Card] = List(card_1, card_2, card_3, card_4, card_5)
}

object PokerHand
{
  def create(cards: List[Card]): Try[PokerHand] = cards match
    {
      case card_1 :: card_2 :: card_3 :: card_4 :: card_5 :: Nil =>
        Success(PokerHand(card_1, card_2, card_3, card_4, card_5))
      case _ => Failure(IncorrectNumberOfCardsException(cards))
    }
}