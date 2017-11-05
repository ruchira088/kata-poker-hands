package cards

sealed trait CardValue {
  def value: Int
}

object CardValue
{
  implicit val cardValueOrdering: Ordering[CardValue] = (x: CardValue, y: CardValue) => x.value - y.value
}

case object Two extends CardValue {
  override def value = 2
}

case object Three extends CardValue {
  override def value = 3
}

case object Four extends CardValue {
  override def value = 4
}

case object Five extends CardValue {
  override def value = 5
}

case object Six extends CardValue {
  override def value = 6
}

case object Seven extends CardValue {
  override def value = 7
}

case object Eight extends CardValue {
  override def value = 8
}

case object Nine extends CardValue {
  override def value = 9
}

case object Ten extends CardValue {
  override def value = 10
}

case object Jack extends CardValue {
  override def value = 11
}

case object Queen extends CardValue {
  override def value = 12
}

case object King extends CardValue {
  override def value = 13
}

case object Ace extends CardValue {
  override def value = 14
}