package poker.cards

import poker.exceptions.CardValueMappingException

import scala.util.Try

sealed trait CardValue extends StringMapper[CardValue]
{
  cardValue =>

  def intValue: Int

  override def key: String = intValue.toString

  override def value: CardValue = cardValue
}

object CardValue
{
  private lazy val stringMappings: Map[String, CardValue] =
    StringMapper.mapping(List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace))

  def parseCardValue(valueString: String): Try[CardValue] =
    StringMapper.findValue(stringMappings)(valueString, CardValueMappingException(valueString))
}

case object Two extends CardValue {
  override def intValue = 2
}

case object Three extends CardValue {
  override def intValue = 3
}

case object Four extends CardValue {
  override def intValue = 4
}

case object Five extends CardValue {
  override def intValue = 5
}

case object Six extends CardValue {
  override def intValue = 6
}

case object Seven extends CardValue {
  override def intValue = 7
}

case object Eight extends CardValue {
  override def intValue = 8
}

case object Nine extends CardValue {
  override def intValue = 9
}

case object Ten extends CardValue {
  override def intValue = 10
}

case object Jack extends CardValue {
  override def intValue = 11

  override def key = "J"
}

case object Queen extends CardValue {
  override def intValue = 12

  override def key = "Q"
}

case object King extends CardValue {
  override def intValue = 13

  override def key = "K"
}

case object Ace extends CardValue {
  override def intValue = 14

  override def key = "A"
}