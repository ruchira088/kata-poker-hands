package cards

import utils.ScalaUtils

import scala.util.Try

case class Card(suite: CardSuite, value: CardValue)

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