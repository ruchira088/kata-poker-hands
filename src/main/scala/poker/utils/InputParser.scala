package poker.utils

import java.nio.file.{Path, Paths}

import poker.cards.Card
import poker.exceptions.InputParseException
import poker.game
import poker.game.{Player, PokerGame}
import poker.hand.PokerHand

import scala.util.{Failure, Success, Try}

object InputParser
{
  val PLAYER_DELIMITER = "  "
  val CARD_DELIMITER = " "

  def parseLine(line: String): Try[PokerGame] =
    line.split(PLAYER_DELIMITER).toList.map(parsePlayer) match
      {
        case players if players.length == 2 =>
          ScalaUtils.sequence(players)
            .fold(
              Failure(_),
              { case player_1 :: player_2 :: _ => Success(game.PokerGame(player_1, player_2)) }
            )

        case _ => Failure(InputParseException(line))
      }

  def parsePlayer(playerString: String): Try[Player] = playerString.split(":").toList match
    {
      case name :: cards :: Nil => parseCards(cards.trim).map(Player(name, _))
      case _ => Failure(InputParseException(playerString))
    }

  def parseCards(cardsString: String): Try[PokerHand] = for
    {
      cards <- ScalaUtils.sequence(cardsString.split(CARD_DELIMITER).toList.map(Card.parse))

      pokerHand <- PokerHand.create(cards)
    }
    yield pokerHand
}