package cards

import exceptions.CardValueMappingException

import scala.util.Try

sealed trait CardValue extends Mapper[CardValue]
{
  cardValue =>

  def intValue: Int

  def toSymbol: String = intValue.toString

  override def key: String = toSymbol

  override def value: CardValue = cardValue
}

object CardValue
{
  implicit val cardValueOrdering: Ordering[CardValue] = (x: CardValue, y: CardValue) => x.intValue - y.intValue

  private lazy val stringMappings: Map[String, CardValue] =
    Mapper.mapping(List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace))

  def parseCardValue(valueString: String): Try[CardValue] =
    Mapper.findValue(stringMappings)(valueString, CardValueMappingException(valueString))
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

  override def toSymbol = "J"
}

case object Queen extends CardValue {
  override def intValue = 12

  override def toSymbol = "Q"
}

case object King extends CardValue {
  override def intValue = 13

  override def toSymbol = "K"
}

case object Ace extends CardValue {
  override def intValue = 14

  override def toSymbol = "A"
}