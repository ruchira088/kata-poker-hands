package cards

sealed trait CardSuite

case object Heart extends CardSuite {
  override def toString = "\u2665"
}

case object Spade extends CardSuite {
  override def toString = "\u2660"
}

case object Diamond extends CardSuite {
  override def toString = "\u2666"
}

case object Club extends CardSuite {
  override def toString = "\u2663"
}