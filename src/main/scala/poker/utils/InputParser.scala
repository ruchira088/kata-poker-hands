package poker.utils

import poker.cards.Card
import poker.exceptions.{DuplicateCardException, InputParseException}
import poker.game
import poker.game.{Player, PokerGame}
import poker.hand.PokerHand

import scala.util.{Failure, Success, Try}

object InputParser
{
  val PLAYER_DELIMITER = "  "
  val CARD_DELIMITER = " "
  val NAME_CARDS_SEPARATOR = ":"

  def parseLine(line: String): Try[PokerGame] =
    line.split(PLAYER_DELIMITER).toList.map(parsePlayer) match
      {
        case players if players.length == 2 =>
          ScalaUtils.sequence(players).fold(Failure(_),
              {
                case player_1 :: player_2 :: _ =>
                  if (player_1.pokerHand.cards.intersect(player_2.pokerHand.cards).isEmpty)
                    Success(game.PokerGame(player_1, player_2))
                  else
                    Failure(DuplicateCardException(player_1.pokerHand.cards.intersect(player_2.pokerHand.cards)))
              }
            )

        case _ => Failure(InputParseException(s"""\"$line\" reason: Does NOT contain PLAYER_DELIMITER: \"$PLAYER_DELIMITER\""""))
      }

  def parsePlayer(playerString: String): Try[Player] = playerString.split(NAME_CARDS_SEPARATOR).toList match
    {
      case name :: cards :: Nil => parseCards(cards.trim).map(Player(name, _))
      case _ => Failure(InputParseException(s"""\"$playerString\" reason: Does NOT contain NAME_CARDS_SEPARATOR: \"$NAME_CARDS_SEPARATOR\" """))
    }

  def parseCards(cardsString: String): Try[PokerHand] = for
    {
      cards <- ScalaUtils.sequence(cardsString.split(CARD_DELIMITER).toList.map(Card.parse))

      pokerHand <- PokerHand.create(cards)
    }
    yield pokerHand
}
