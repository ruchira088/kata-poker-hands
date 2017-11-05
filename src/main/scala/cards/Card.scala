package cards

case class Card(suite: CardSuite, value: CardValue)
{
  def isSameSuite(card: Card): Boolean = suite == card.suite

  def isSameValue(card: Card): Boolean = value == card.value
}