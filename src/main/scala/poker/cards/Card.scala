package poker.cards

import scala.util.Try

case class Card(suite: CardSuite, value: CardValue) {
  override def toString = s"${value.key}$suite"
}

object Card
{
  implicit val ordering: Ordering[Card] =
    (card_1: Card, card_2: Card) => card_2.value.intValue - card_1.value.intValue

  def parse(string: String): Try[Card] = for
    {
      suite <- CardSuite.parseCardSuite(string.last.toString)
      value <- CardValue.parseCardValue(string.init)
    }
    yield Card(suite, value)

  def compareCardList(cards_1: List[Card], cards_2: List[Card]): Int = (cards_1, cards_2) match
  {
    case (_, Nil) => 0
    case (Nil, _) => 0
    case (Card(_, x) :: _, Card(_, y) :: _) if x != y => x.intValue - y.intValue
    case (_ :: xs, _ :: ys) => compareCardList(xs, ys)
  }

  def printCards(cardsList: List[Card]) = cardsList.mkString("[", ",", "]")
}