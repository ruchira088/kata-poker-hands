package poker.cards

import poker.exceptions.CardSuiteMappingException

import scala.util.Try

sealed trait CardSuite extends StringMapper[CardSuite] {
  cardSuit =>

  override def value: CardSuite = cardSuit
}

object CardSuite
{
  private lazy val mappings = StringMapper.mapping(List(Heart, Spade, Diamond, Club))

  def parseCardSuite(stringValue: String): Try[CardSuite] =
    StringMapper.findValue(mappings)(stringValue, CardSuiteMappingException(stringValue))
}

case object Heart extends CardSuite {
  override def toString = "\u2665"

  override def key = "H"
}

case object Spade extends CardSuite {
  override def toString = "\u2660"

  override def key = "S"
}

case object Diamond extends CardSuite {
  override def toString = "\u2666"

  override def key = "D"
}

case object Club extends CardSuite {
  override def toString = "\u2663"

  override def key = "C"
}