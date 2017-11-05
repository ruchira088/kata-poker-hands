import cards._
import org.scalatest.{FlatSpec, Matchers}

class PokerHandSpec extends FlatSpec with Matchers
{
  "PokerHand without a flush" should "return None" in
  {
    val cardsList = List(
      Card(Heart, Two),
      Card(Diamond, Ace),
      Card(Spade, Four),
      Card(Club, Jack),
      Card(Heart, Ace)
    )

    WinningCards.getFlush(cardsList) shouldEqual None
  }

  // TODO
  "PokerHand with a flush" should "return ???" in
  {
    val cardsList = List(
      Card(Heart, Two),
      Card(Heart, Ace),
      Card(Heart, Four),
      Card(Heart, Jack),
      Card(Heart, King)
    )

    WinningCards.getFlush(cardsList) shouldEqual Some(cardsList)
  }

  "PokerHand with a pair" should "the pair" in
  {
    println(WinningCards.getPairs(List(
      Card(Heart, Jack),
      Card(Diamond, Jack),
      Card(Spade, Nine),
      Card(Heart, Ten),
      Card(Heart, Nine),
      Card(Club, Nine)
    )))
  }

  "PokerHand with a straight" should "the straight" in
  {
    println(WinningCards.isStraight(
      List(
        Card(Heart, Ace),
        Card(Diamond, King),
        Card(Heart, Ten),
        Card(Heart, Jack),
        Card(Club, Queen)
      )
    ))
  }

  "PokerHand with a full house" should "full house" in
  {
    println(WinningCards.isFullHouse(
      List(
        Card(Heart, Three),
        Card(Club, Two),
        Card(Diamond, Two),
        Card(Spade, Three),
        Card(Club, Three)
      )
    ))
  }

  "PokerHand with a straight flush" should "straight flush" in {
    println(WinningCards.isStraightFlush(
      List(
        Card(Heart, Ace),
        Card(Heart, Queen),
        Card(Heart, Jack),
        Card(Heart, King),
        Card(Heart, Ten)
      )
    ))
  }
}
