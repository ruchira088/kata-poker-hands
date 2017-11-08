package cards

import utils.ScalaUtils

import scala.util.Try

case class Card(suite: CardSuite, value: CardValue)
{
  def isSameSuite(card: Card): Boolean = suite == card.suite

  def isSameValue(card: Card): Boolean = value == card.value
}

object Card
{
  def parse(string: String): Try[Card] = for
    {
      suiteString <- ScalaUtils.toTry(string.reverse.headOption)
      suite <- CardSuite.parseCardSuite(suiteString.toString)

      value <- CardValue.parseCardValue(string.reverse.tail)
    }
    yield Card(suite, value)
}