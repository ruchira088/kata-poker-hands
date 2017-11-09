package poker.hand

import org.scalatest.{FlatSpec, Matchers}
import poker.cards.{Card, _}

class HighestRankingCardsSpec extends FlatSpec with Matchers
{
  implicit class CardWrapper(card: Card) {
    def &(anotherCard: Card): CardList = CardList(List(card, anotherCard))
  }

  case class CardList(cards: List[Card]) {
    def &(card: Card): CardList = CardList(cards :+ card)
  }

  object CardList {
    implicit def toList(cardList: CardList): List[Card] = cardList.cards
  }

  val nonMatchingCardList: List[Card] =
    Card(Heart, Two) & Card(Diamond, Ace) & Card(Spade, Four) & Card(Club, Jack) & Card(Heart, Queen)

  "StraightFlush" should "return None when the nonMatchingCardList is passed" in
  {
    HighestRankingCards getStraightFlush nonMatchingCardList shouldEqual None
  }

  it should "return Some(List(Card*)) when sequenced cards of the same suite are passed" in
  {
    val cards = Card(Heart, Ten) & Card(Heart, Ace) & Card(Heart, Jack) & Card(Heart, King) & Card(Heart, Queen)

    HighestRankingCards getStraightFlush cards shouldEqual Some(List(Ace, King, Queen, Jack, Ten).map(Card(Heart, _)))
  }

  it should "return None when sequenced cards of different suites are passed" in
  {
    val cards = Card(Heart, Ten) & Card(Club, Ace) & Card(Heart, Jack) & Card(Heart, King) & Card(Heart, Queen)

    HighestRankingCards getStraightFlush cards shouldEqual None
  }

  "FourOfAKind" should "return None when the nonMatchingCardList is passed" in
  {
    HighestRankingCards fourOfAKind nonMatchingCardList shouldEqual None
  }

  it should "return Some(List(Card*)) when 4 of a kind is passed" in
  {
    val cards = Card(Heart, Eight) & Card(Club, Eight) & Card(Spade, Eight) & Card(Diamond, Eight) & Card(Spade, Two)

    HighestRankingCards fourOfAKind cards shouldEqual Some(List(Heart, Club, Spade, Diamond).map(Card(_, Eight)))
  }

  "FullHouse" should "return None when the nonMatchingCardList is passed" in
  {
    HighestRankingCards getFullHouse nonMatchingCardList shouldEqual None
  }

  it should "return Some(List(List(Card*))) when fullHouse card list is passed" in
  {
    val cards = Card(Heart, Ten) & Card(Diamond, Jack) & Card(Club, Ten) & Card(Spade, Ten) & Card(Heart, Jack)

    HighestRankingCards getFullHouse cards shouldEqual Some(List((Card(Heart, Ten) & Card(Club, Ten) & Card(Spade, Ten)).cards, (Card(Diamond, Jack) & Card(Heart, Jack)).cards))
  }

  "Flush" should "return None when the nonMatchingCardList is passed" in
  {
    HighestRankingCards getFlush nonMatchingCardList shouldEqual None
  }

  it should "return Some(List(Card*)) when a flush is passed" in
  {
    val cards = List(Ace, Jack, Two, Ten, King).map(Card(Spade, _))

    HighestRankingCards getFlush cards shouldEqual Some(cards)
  }

  "Straight" should "return None when the nonMatchingCardList is passed" in
  {
    HighestRankingCards getStraight nonMatchingCardList shouldEqual None
  }

  it should "return Some(List(Card*)) when a straight is passed" in
  {
    val cards = Card(Heart, Ten) & Card(Spade, Jack) & Card(Club, Nine) & Card(Diamond, Eight) & Card(Heart, Seven)

    HighestRankingCards getStraight cards shouldEqual Some(cards.sorted)
  }

  it should "return Some(List(Card*)) when a straight with lower bound Ace is passed" in
  {
    val cards = Card(Heart, Four) & Card(Spade, Ace) & Card(Club, Five) & Card(Diamond, Two) & Card(Heart, Three)

    HighestRankingCards getStraight cards shouldEqual Some((Card(Club, Five) & Card(Heart, Four) & Card(Heart, Three) & Card(Diamond, Two) & Card(Spade, Ace)).cards)
  }

  "ThreeOfAKind" should "return None when the nonMatchingCardList is passed" in
  {
    HighestRankingCards threeOfAKind nonMatchingCardList shouldEqual None
  }

  it should "return Some(List(Card*)) when three of a kind is passed" in
  {
    val cards = Card(Heart, Four) & Card(Diamond, Two) & Card(Heart, Three) & Card(Spade, Four) & Card(Club, Four)

    HighestRankingCards threeOfAKind cards shouldEqual Some((Card(Heart, Four) &Card(Spade, Four) & Card(Club, Four)).cards)
  }
}
