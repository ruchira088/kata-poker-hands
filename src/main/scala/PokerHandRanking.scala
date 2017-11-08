trait PokerHandRanking extends Ordered[PokerHandRanking]
{
  def rankValue: Int

  override def compare(that: PokerHandRanking): Int = rankValue - that.rankValue
}

case object HighCard extends PokerHandRanking {
  override def rankValue: Int = 5
}

case object Pair extends PokerHandRanking {
  override def rankValue: Int = 6
}

case object TwoPair extends PokerHandRanking {
  override def rankValue: Int = 7
}

case object ThreeOfAKind extends PokerHandRanking {
  override def rankValue: Int = 8
}

case object Straight extends PokerHandRanking {
  override def rankValue: Int = 9
}

case object Flush extends PokerHandRanking {
  override def rankValue: Int = 10
}

case object FullHouse extends PokerHandRanking {
  override def rankValue: Int = 11
}

case object FourOfAKind extends PokerHandRanking {
  override def rankValue: Int = 12
}

case object StraightFlush extends PokerHandRanking {
  override def rankValue: Int = 13
}